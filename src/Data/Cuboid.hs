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
  , fromSizedVectors
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
import qualified Data.Vector.Sized.X as SV

newtype Cuboid (z :: Nat) (y :: Nat) (x :: Nat) a = Cuboid
  (SV.Vector z (SV.Vector y (SV.Vector x a)))
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

type Cube n   = Cuboid n n n
type Nil      = Cuboid 0 0 0
type Line     = Rect 1
type Point    = Line 1
type Rect     = Cuboid 1
type Square n = Rect n n

instance ( KnownNat z
         , KnownNat y
         , KnownNat x
         )
    => Applicative (Cuboid z y x) where
  pure x = Cuboid (pure (pure (pure x)))

  Cuboid f <*> Cuboid x = Cuboid
    $ SV.zipWith
       (SV.zipWith
          (SV.zipWith ($))) f x

instance Each (Cuboid z y x a) (Cuboid z y x b) a b

instance Render a => Render (Cuboid z y x a) where
  render = toTexts >>> map renderAisle >>> T.intercalate "\n"
    where
      renderAisle :: [[Text]] -> Text
      renderAisle = map Table.Row
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

fromSizedVectors :: forall z y x a.
                    SV.Vector z (SV.Vector y (SV.Vector x a))
                 -> Cuboid z y x a
fromSizedVectors = Cuboid

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
  (\(Cuboid v) -> (v !! 0) !! c)
  (\(Cuboid aisles) col' -> Cuboid $ aisles //
    [(0, (aisles !! 0) // [(c, col')])])

row :: forall y x a.
       ( KnownNat y
       , KnownNat x
       )
    => ( Finite x )
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
slice (z, y, x) = lens (sliceZ z . sliceY y . sliceX x) set'
  where
    set' :: Cuboid z  y  x  b
         -> Cuboid z' y' x' b
         -> Cuboid z  y  x  b
    set' (Cuboid aisles) (Cuboid aisles') = Cuboid aisles''
      where
        aisles'' :: SV.Vector z (SV.Vector y (SV.Vector x b))
        aisles'' = SV.update aisles updates
          where
            updates :: SV.Vector z' (Int, SV.Vector y (SV.Vector x b))
            updates = SV.zipWith f indexVec aisles'
              where
                indexVec = SV.unsafeFromList indexes

                indexes :: [Int]
                indexes = [start'..stop']

                start' :: Int
                start' = z_i

                stop' :: Int
                stop'  = start' + z'i

                z'i :: Int
                z'i = extractInt (Proxy @z')

                z_i :: Int
                z_i = extractInt (Proxy @z_)

                f :: Int
                  -> (SV.Vector y' (SV.Vector x' b))
                  -> (Int, SV.Vector y (SV.Vector x b))
                f n _v' = (n, v'')
                  where
                    v'' = SV.update something updates'

                    something :: SV.Vector y (SV.Vector x b)
                    something = panic "aisles''.something"

                    updates' :: SV.Vector rr (Int, a)
                    updates' = panic "aisles''.updates"

    -- set' = over f
    --   where
    --     f :: SV.Vector z y x b
    --       -> SV.Vector z y x b
    --     f = undefined

-- slice :: forall sz sy sx z y x z' y' x'
--                 r' c' r c m n
--                 b.
--          ( KnownNat sz
--          , KnownNat sy
--          , KnownNat sx
--          , KnownNat z'
--          , KnownNat y'
--          , KnownNat x'
--          , KnownNat z
--          , KnownNat y
--          , KnownNat x
--          , x ~ ((sx + r') + m)
--          , y ~ ((sy + c') + n)
--          )
--       => (Proxy sz, Proxy sy, Proxy sz)
--       -> Lens' (Cuboid z  y  x  b)
--                (Cuboid z' y' x' b)
-- slice (cp, rp, ap) = lens (sliceA ap . sliceC cp . sliceR rp) set'
--   where
--     set' :: Cuboid c r a b
--          -> Cuboid r' c' a' b
--          -> Cuboid r c a a
--     set' = panic "Cuboid.slice.set'"

    -- set' (Cuboid v) (Cuboid v') = Cuboid v''
    --   where
    --     _ = v  :: SV.Vector c  (SV.Vector r  a)
    --     _ = v' :: SV.Vector c' (SV.Vector r' a)

    --     v'' :: SV.Vector c (SV.Vector r a)
    --     v'' = SV.update v updates
    --       where
    --         updates :: SV.Vector c' (Int, SV.Vector r a)
    --         updates = SV.zipWith f indexes v'
    --           where
    --             f :: Int
    --               -> SV.Vector r' a
    --               -> (Int, SV.Vector r a)
    --             f ci vr = (ci, vv')
    --               where

    --                 vv :: SV.Vector r a
    --                 vv = SV.index v ci'

    --                 vv' :: SV.Vector r a
    --                 vv' = SV.update vv updates'
    --                   where
    --                     updates' :: SV.Vector r' (Int, a)
    --                     updates' = SV.zipWith (,) indexes' vr

    --                     indexes' :: SV.Vector r' Int
    --                     indexes' = SV.unsafeFromList [ start' .. stop' ]
    --                       where
    --                         start' = fromIntegral . natVal $ Proxy @sx
    --                         stop'  = fromIntegral . natVal $ Proxy @r'

    --                 ci' = case packFinite @c (fromIntegral ci) of
    --                         Just x -> x
    --                         Nothing -> panic "Failed when packing finite"

    --             indexes :: SV.Vector c' Int
    --             indexes = SV.unsafeFromList [ start' .. stop' ]
    --               where
    --                 start' = fromIntegral . natVal $ Proxy @sy
    --                 stop'  = fromIntegral . natVal $ Proxy @c'

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
  . SV.zipWith (SV.++) (a !! 0)
  $ b !! 0

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
          -> Rect x y a -- TODO Hrmmm
transpose = over (SV.map SV.transpose)

over :: forall z' y' x' a' z y x a.
         (  (SV.Vector z (SV.Vector y (SV.Vector x a)))
         -> (SV.Vector z' (SV.Vector y' (SV.Vector x' a')))
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
