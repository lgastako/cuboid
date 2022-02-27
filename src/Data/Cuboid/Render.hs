{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cuboid.Render
  ( Render(..)
  ) where

import Data.Cuboid.Prelude

import qualified Data.Text as T

class Render r where
  render :: r -> Text

instance Render Text where
  render = identity

instance Render () where
  render = show

instance Render Int where
  render = show

instance Render Integer where
  render = show

instance Render Float where
  render = show

instance Render Double  where
  render = show

instance Render Bool where
  render = show

instance Render Char where
  render = show

instance Render a => Render (Ratio a) where
  render x = render (numerator x) <> " % " <> render (denominator x)

instance Render a => Render (Maybe a) where
  render x = "Maybe " <> render x

instance (Render e, Render a) => Render (Either e a) where
  render = \case
    Left  e -> "Left "  <> render e
    Right a -> "Right " <> render a

instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <> render a <> "," <> render b <> ")"

instance Render a => Render [a] where
  render = map render
       >>> T.intercalate ","
       >>> ("[" <>) . (<> "]")  -- << --  Her eyes stare at you from the abyss.
