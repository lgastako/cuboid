{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cuboid.CSV
  ( -- readCSV
    read
  -- , writeCSV
  , write
  ) where

import Data.Cuboid.Prelude

import Data.Cuboid

data Error = FileNotFound
  deriving (Eq, Generic, Ord, Show)

instance Exception Error

read :: FilePath -> IO (Either Error (Cuboid z y x a))
read = panic "CSV.read"

readTexts :: FilePath -> Either Error [[Text]]
readTexts = panic "CSV.readTexts"

write :: FilePath -> Cuboid z y x a -> IO ()
write = panic "CSV.write"
