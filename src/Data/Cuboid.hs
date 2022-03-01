{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Cuboid
  ( module Control.Applicative
  , module Data.Foldable
  , module Data.Functor
  , module Data.Traversable
  , module Data.Slice.Lens
  , Cube
  , Cuboid
  , Line
  , Nil
  , Point
  , Rect
  , Square
  -- Constructors
  , empty
  , emptyX
  , emptyY
  , emptyZ
  , enumFrom
  , fromList
  , fromUnsizedVectors
  , fromSizedVectors
  , fromTexts
  -- Optics
  , at
  , col
  , row
  , slice
  , sliceX
  , sliceY
  , sliceZ
  , transposed
  -- Combinators
  , (<+>)
  , (</>)
  , transpose
  , zipWith
  , zip
  -- Eliminators
  , toLists
  , toTexts
  , unCuboid
  , unsafeFromList
  ) where

import Data.Cuboid.Prelude hiding ( empty
                                  , enumFrom
                                  , transpose
                                  , zip
                                  , zipWith
                                  )

import Data.Cuboid.Render         ( Render( render ) )
import Data.List.Split            ( chunksOf )
import Data.Vector.Sized.X        ( (!!)
                                  , (//)
                                  )
import Data.Slice.Lens            ( Slice(..)
                                  , sliced
                                  , sliced'
                                  )

import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Traversable

import qualified Data.Cuboid.Table   as Table
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Vector         as U
import qualified Data.Vector.Sized.X as SV

newtype Cuboid (z :: Nat) (y :: Nat) (x :: Nat) a = Cuboid
  (SV.Vector z (SV.Vector y (SV.Vector x a)))
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

type Cube n   = Cuboid n n n
type Line     = Rect 1
type Nil      = Cuboid 0 0 0
type Point    = Line 1
type Rect     = Cuboid 1
type Square n = Rect n n

instance ( KnownNat z
         , KnownNat y
         , KnownNat x
         )
    => Applicative (Cuboid z y x) where
  pure = Cuboid . pure . pure . pure

  Cuboid f <*> Cuboid x = Cuboid
    $ SV.zipWith
       (SV.zipWith
          (SV.zipWith ($))) f x

instance Each (Cuboid z y x a) (Cuboid z y x b) a b

instance Render a => Render (Cuboid z y x a) where
  render = toTexts >>> map renderPage >>> T.intercalate "\n"
    where
      renderPage :: [[Text]] -> Text
      renderPage = map Table.Row
               >>> Table.fromRows
               >>> Table.render

-- ================================================================ --
--   Constructors
-- ================================================================ --

empty :: Cuboid 0 0 0 a
empty = Cuboid SV.empty

emptyZ :: Cuboid 0 y x a
emptyZ = Cuboid SV.empty

emptyY :: KnownNat z => Cuboid z 0 x a
emptyY = Cuboid . pure $ SV.empty

emptyX :: (KnownNat z, KnownNat y) => Cuboid z y 0 a
emptyX = Cuboid . pure . pure $ SV.empty

fromList :: forall z y x a.
            ( KnownNat z
            , KnownNat y
            , KnownNat x
            )
         => [[[a]]]
         -> Maybe (Cuboid z y x a)
fromList xs
  | nSizes == 1 = Just $ unsafeFromList xs
  | otherwise   = Nothing
  where
    nSizes = length . L.nub . map length $ xs

fromUnsizedVectors :: forall z y x a.
                      ( KnownNat z
                      , KnownNat y
                      , KnownNat x
                      )
                   => U.Vector (U.Vector (U.Vector a))
                   -> Maybe (Cuboid z y x a)
fromUnsizedVectors uuu = do
  uus <- (traverse . traverse) SV.toSized uuu
  uss <- traverse SV.toSized uus
  sss <- SV.toSized uss
  pure $ Cuboid sss

fromSizedVectors :: forall z y x a.
                    SV.Vector z (SV.Vector y (SV.Vector x a))
                 -> Cuboid z y x a
fromSizedVectors = Cuboid

fromTexts :: forall y x.
             ( KnownNat y
             , KnownNat x
             )
          => [[Text]]
          -> Maybe (Rect y x Text)
fromTexts = fromList . pure

enumFrom :: forall z y x k.
            ( Enum k
            , KnownNat z
            , KnownNat y
            , KnownNat x
            )
         => k
         -> Cuboid z y x k
enumFrom k = [k..]
  |> ( chunksOf x
   >>> chunksOf y
   >>> take z
   >>> unsafeFromList
     )
  where
    x = extractInt (Proxy @x)
    y = extractInt (Proxy @y)
    z = extractInt (Proxy @z)

unsafeFromList :: forall z y x a.
                  ( KnownNat z
                  , KnownNat y
                  , KnownNat x
                  )
               => [[[a]]]
               -> Cuboid z y x a
unsafeFromList = (map . map) SV.unsafeFromList
             >>> map SV.unsafeFromList
             >>> SV.unsafeFromList
             >>> SV.transpose
             >>> fromSizedVectors

-- ================================================================ --
--   Optics
-- ================================================================ --

at :: forall z y x a.
      ( KnownNat z
      , KnownNat y
      , KnownNat x
      )
   => ( Finite z
      , Finite y
      , Finite x
      )
   -> Lens' (Cuboid z y x a) a
at (z, y, x) = lens
  (\(Cuboid v) -> ((v !! z) !! y) !! x)
  (\(Cuboid v) x' -> Cuboid
    $ v //
        [(z, v !! z //
                    [(y, (v !! z) !! y
                       //
                       [(x, x')])])])

col :: forall y x a.
       Finite y
    -> Lens' (Rect y x a)
             (SV.Vector x a)
col c = lens
  (\(Cuboid v) -> SV.head v !! c)
  (\(Cuboid pages) col' -> Cuboid $ pages //
    [(0, SV.head pages // [(c, col')])])

row :: forall y x a.
       ( KnownNat y
       , KnownNat x
       )
    => Finite x
    -> Lens' (Rect y x a)
             (SV.Vector y a)
row c = transposed . col c

sliceZ :: forall z' z y x m p q a.
          ( KnownNat q
          , KnownNat z'
          , z ~ ((q + z') + m)
          )
       => p q
       -> Cuboid z  y x a
       -> Cuboid z' y x a
sliceZ = over . SV.slice

sliceY :: forall y' z y x m p q a.
          ( KnownNat y'
          , KnownNat q
          , y ~ ((q + y') + m)
          )
       => p q
       -> Cuboid z y  x a
       -> Cuboid z y' x a
sliceY pq = over $ SV.map (SV.slice pq)

sliceX :: forall x' z y x m p q a.
          ( KnownNat q
          , KnownNat x'
          , x ~ ((q + x') + m)
          )
       => p q
       -> Cuboid z y x  a
       -> Cuboid z y x' a
sliceX pq = over $ SV.map (SV.map (SV.slice pq))

slice :: forall z'
                y' x' z_ y_ x_ z y x m n o p b.
         ( KnownNat z'
         , KnownNat z_
         , KnownNat y'
         , KnownNat y_
         , KnownNat x_
         , KnownNat x'
         , z ~ ((z_ + z') + m)
         , y ~ ((y_ + y') + n)
         , x ~ ((x_ + x') + o)
         )
      => ( p z_
         , p y_
         , p x_
         )
      -> Lens' (Cuboid z  y  x  b)
               (Cuboid z' y' x' b)
slice (z, y, x) = lens (sliceZ z. sliceY y . sliceX x) set'
  where
    set' :: Cuboid z  y  x  b
         -> Cuboid z' y' x' b
         -> Cuboid z  y  x  b
    set' (Cuboid pages) (Cuboid _pages') = Cuboid $ SV.map f pages
      where
        f :: SV.Vector y (SV.Vector x b)
          -> SV.Vector y (SV.Vector x b)
        f = SV.map g
          where
            g :: SV.Vector x b
              -> SV.Vector x b
            g row' = SV.update row' rowUpdates
              where
                rowUpdates :: SV.Vector x' (Int, b)
                rowUpdates = panic "slice.rowUpdates"

transposed :: forall y x a.
              ( KnownNat y
              , KnownNat x
              )
           => Iso' (Rect y x a)
                   (Rect x y a)
transposed = iso transpose transpose

-- ================================================================ --
--   Combinators
-- ================================================================ --

(<+>) :: forall y xa xb xc a.
         ( xc ~ (xa + xb) )
      => Rect y xa a
      -> Rect y xb a
      -> Rect y xc a
Cuboid a <+> Cuboid b = Cuboid
  . SV.singleton
  . SV.zipWith (SV.++) (SV.head a)
  . SV.head
  $ b

(</>) :: forall ya yb yc x a.
         ( yc ~ (ya + yb) )
      => Rect ya x a
      -> Rect yb x a
      -> Rect yc x a
Cuboid a </> Cuboid b = Cuboid $ SV.zipWith (SV.++) a b

transpose :: forall y x a.
             ( KnownNat y
             , KnownNat x
             )
          => Rect y x a
          -> Rect x y a
transpose = over (SV.map SV.transpose)

over :: forall z' y' x' a' z y x a.
         (  SV.Vector z  (SV.Vector y  (SV.Vector x  a))
         -> SV.Vector z' (SV.Vector y' (SV.Vector x' a'))
         )
      -> Cuboid z  y  x  a
      -> Cuboid z' y' x' a'
over f (Cuboid v) = Cuboid (f v)

zipWith :: forall z y x a b c.
           ( KnownNat z
           , KnownNat y
           , KnownNat x
           )
        => (a -> b -> c)
        -> Cuboid z y x a
        -> Cuboid z y x b
        -> Cuboid z y x c
zipWith f a b = f <$> a <*> b

zip :: forall z y x a b.
       ( KnownNat z
       , KnownNat y
       , KnownNat x
       )
    => Cuboid z y x a
    -> Cuboid z y x b
    -> Cuboid z y x (a, b)
zip = zipWith (,)

-- ================================================================ --
--   Eliminators
-- ================================================================ --

toLists :: forall z y x a.
           Cuboid z y x a
        -> [[[a]]]
toLists = unCuboid
      >>> map (map SV.toList)
      >>> map SV.toList
      >>> SV.toList

toTexts :: forall z y x a.
           Render a
        => Cuboid z y x a
        -> [[[Text]]]
toTexts = toLists >>> (map . map . map) render

unCuboid :: Cuboid z y x a -> SV.Vector z (SV.Vector y (SV.Vector x a))
unCuboid (Cuboid x) = x

-- ================================================================ --
--   Helpers
-- ================================================================ --

extractInt :: forall n. KnownNat n => Proxy n -> Int
extractInt = fromIntegral . natVal
