{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Cuboid.CSV
  ( read
  ) where

import Data.Cuboid.Prelude
import Data.Csv

import Data.Cuboid ( Cuboid )

import qualified Data.Cuboid as Cube
import qualified Data.Text   as T
import qualified Data.Vector as U

read :: forall z y x m.
        ( MonadIO m
        , KnownNat z
        , KnownNat y
        , KnownNat x
        )
     => FilePath
     -> m (Maybe (Cuboid z y x Text))
read path = readTexts path >>= either
  (const $ pure Nothing)
  (pure . Cube.fromUnsizedVectors . pure)

readTexts :: forall m.
             MonadIO m
          => FilePath-> m (Either Text (U.Vector (U.Vector Text)))
readTexts = liftIO
     . map ( ensureSameRowLengths
           . first T.pack
           . decode NoHeader
           . cs
           )
     . readFile

ensureSameRowLengths :: Either Text (U.Vector (U.Vector Text))
                     -> Either Text (U.Vector (U.Vector Text))
ensureSameRowLengths = \case
  left@Left {} -> left
  right@(Right ((length &&& identity) -> (n, rows)))
    | null rows -> right
    | all (== n) . map length $ rows -> right
    | otherwise -> Left "row size mismatch - all rows must be the same length."

