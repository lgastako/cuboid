{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cuboid.Table
  ( Row( Row )
  , Table
  -- Constructors
  , from
  , fromHeadedRows
  , fromRows
  , fromTexts
  -- Combinators
  , promoteHeader
  -- Eliminators
  , display
  , render
  ) where

import Data.Cuboid.Prelude hiding ( from )

import           Data.Cuboid.Widths as Widths
import qualified Data.Text          as T

newtype Table = Table (Maybe Header, [Row])
  deriving (Eq, Generic, Ord, Show)

newtype Header = Header { unHeader :: Row }
  deriving (Eq, Generic, Ord, Show)

newtype Row = Row {unRow :: [Text]}
  deriving (Eq, Generic, Ord, Show)

-- ================================================================ --
--   Constructors
-- ================================================================ --

from :: Maybe Header -> [Row] -> Table
from = curry Table

fromHeadedRows :: [Row] -> Maybe Table
fromHeadedRows (header:rows) = Just (from (Just $ Header header) rows)
fromHeadedRows []            = Nothing

fromRows :: [Row] -> Table
fromRows = from Nothing

fromTexts :: [[Text]] -> Table
fromTexts = fromRows . map Row

-- ================================================================ --
--   Combinators
-- ================================================================ --

promoteHeader :: Table -> Maybe Table
promoteHeader = \case
  Table (_, [])   -> Nothing
  Table (_, x:xs) -> Just $ Table (Just (Header x), xs)

-- ================================================================ --
--  Eliminators
-- ================================================================ --

display :: Table -> IO ()
display = putStr . render

render :: Table -> Text
render table = (headerText <>)
  . T.unlines
  . map (joinCols . Widths.pad ws)
  $ rowTexts
  where
    ws = widths ts
    ts = toTexts table

    headerText :: Text
    headerText = case table of
      Table (Nothing, _) -> ""
      Table (Just (Header hRow), _) -> joinCols (Widths.pad ws (unRow hRow))
        <> horizontalSeperator

    horizontalSeperator = T.intercalate sepSep (Widths.fill ws "-")
      `between` "\n"

    sep    = " | "
    sepSep = "-+-"

    rowTexts = case table of
      Table (_, rows) -> map unRow rows

    joinCols = T.intercalate sep

-- ================================================================ --
--  Helpers
-- ================================================================ --

toTexts :: Table -> [[Text]]
toTexts = \case
  Table (Nothing           , rows) ->             map unRow rows
  Table (Just (Header hRow), rows) -> (unRow hRow:map unRow rows)

between :: Text -> Text -> Text
between burger bun = bun <> burger <> bun
