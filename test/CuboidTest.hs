{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CuboidTest
  ( spec_Point
  , spec_Line
  , spec_Rect
  , spec_Square
  ) where

import Data.Cuboid.Prelude

import Test.Tasty.Hspec  ( Spec
                         , context
                         , hspec
                         , it
                         , shouldBe
                         )
import Data.Cuboid       ( (<+>)
                         , (<//>)
                         , Line
                         , Point
                         , Rect
                         , Square
                         )

import qualified Data.Char   as C
import qualified Data.Cuboid as DC

_ = hspec

spec_Point :: Spec
spec_Point =
  context "given point X" $ do
    let po = pure 'X' :: Point Char

    it "should have length 1" $
      length po `shouldBe` 1

    it "should have toList = ['X']" $
      toList po `shouldBe` "X"

spec_Line :: Spec
spec_Line =
  context "given a line of 3 X's" $ do
    let li = pure 'X' :: Line 3 Char

    it "should have length 3" $
      length li `shouldBe` 3

    it "should have toList = ['X', 'X', 'X']" $
      toList li `shouldBe` "XXX"

spec_Rect :: Spec
spec_Rect =  do
  context "given a (Rect 0 0)" $ do
    let sq = DC.empty :: Rect 0 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    -- it "should have toTexts = []" $
    --   DC.toTexts sq `shouldBe` []

    it "should have toList = []" $
      DC.toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      DC.transpose sq `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should tranpose to itself " $
        DC.transpose sq `shouldBe` sq

  context "given a (Rect 0 N)" $ do
    let sq = truth :: DC.Rect 0 3 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    -- it "should have toTexts = [ [], [], [] ]" $
    --   DC.toTexts sq `shouldBe` [ [], [], [] ]

    it "should have toList []" $
      toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` (truth :: Rect 0 6 Bool)

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        DC.transpose sq `shouldBe` (truth :: DC.Rect 3 0 Bool)

  context "given a (Rect N 0)" $ do
    let sq = truth :: DC.Rect 3 0 Bool

    it "should have length 0" $
      length sq `shouldBe` 0

    -- it "should toTexts []" $
    --   DC.toTexts sq `shouldBe` []

    it "should toList []" $
      DC.toList sq `shouldBe` []

    it "should remain same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should still be itself after being juxtaposed with itself" $
      sq <+> sq `shouldBe` (truth :: DC.Rect 6 0 Bool)

    it "should still be itself after being stacked on top of itself" $
      sq <//> sq `shouldBe` sq

    context "transpose" $
      it "should transpose dimensions " $
        DC.transpose sq `shouldBe` (truth :: DC.Rect 0 3 Bool)

  context "given a (Rect N M) of homogenous values" $ do
    let sq = truth :: DC.Rect 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    -- it "should toTexts []" $
    --   DC.toTexts sq `shouldBe` show <<$>> [[True, True], [True, True]]

    it "should toList []" $
      DC.toList sq `shouldBe` [True, True, True, True]

    it "should remain the same Rect under Applicative" $
      ((&&) <$> sq <*> sq) `shouldBe` sq

    it "should be it's own transpose" $
      DC.transpose sq `shouldBe` sq

    it "should index values properly" $
      sq ^. DC.at (0, 1) `shouldBe` True

  context "given a (Rect N M) of heterogenous values" $ do
    let xs = [[True, False], [False, True]]
        sq = (DC.fromList xs
              & fromMaybe (panic "RectTest:boom")) :: DC.Rect 2 2 Bool

    it "should have length 0" $
      length sq `shouldBe` 4

    -- it "should toTexts []" $
    --   DC.toTexts sq `shouldBe` show <<$>> xs

    it "should toList []" $
      DC.toList sq `shouldBe` concat xs

    it "should remain the same Rect under Applicative" $
      ((&&) <$> sq <*> DC.transpose sq) `shouldBe` sq

    it "should index values properly" $
      ( sq ^. DC.at (0, 1)
      , sq ^. DC.at (1, 0)
      , sq ^. DC.at (0, 0)
      , sq ^. DC.at (1, 1)
      ) `shouldBe` (False, False, True, True)

  context "given a (Rect 4 4) of heterogenous values" $ do
    let xs = [ "ABCD"
             , "EFGH"
             , "IJKL"
             , "MNOP"
             ]
        ys = [ "CD"
             , "GH"
             , "KL"
             , "OP"
             ]
        zs = [ [ 'E', 'F', 'G', 'H' ]
             , [ 'I', 'J', 'K', 'L' ]
             ]
        qs = [ [ 'F', 'G' ]
             , [ 'J', 'K' ]
             ]
        sqx = DC.unsafeFromList xs :: DC.Rect 4 4 Char
        sqy = DC.unsafeFromList ys :: DC.Rect 4 2 Char
        sqz = DC.unsafeFromList zs :: DC.Rect 2 4 Char
        sqq = DC.unsafeFromList qs :: DC.Rect 2 2 Char

    context "given their needs, users" $ do

      it "should be able to operate on columns" $
         (sqx & DC.col 1 . each %~ C.toLower)
           `shouldBe` DC.unsafeFromList
             [ "AbCD"
             , "EfGH"
             , "IjKL"
             , "MnOP"
             ]

      it "should be able to operate on rows" $
         (sqx & DC.row 1 . each %~ C.toLower)
           `shouldBe` DC.unsafeFromList
             [ "ABCD"
             , "efgh"
             , "IJKL"
             , "MNOP"
             ]

    context "sliceC" $
      it "should something or other..." $ do
        let sub :: Rect 4 2 Char
            sub = DC.sliceC (Proxy @2) sqx
        sub `shouldBe` sqy

    context "sliceR" $
      it "should something or other..." $ do
        let sub :: Rect 2 4 Char
            sub = DC.sliceR (Proxy @1) sqx
        sub `shouldBe` sqz

    context "col" $
      context "viewing" $
        it "should work" $
          toList (sqx ^. DC.col 1) `shouldBe` "BFJN"

    context "row" $
      context "viewing" $
        it "should work" $
          toList (sqx ^. DC.row 1) `shouldBe` "EFGH"

    context "slice" $ do

      context "viewing" $
        it "should work as expected" $ do
          let sub :: Rect 2 2 Char
              sub = sqx ^. DC.slice (Proxy @1, Proxy @1)
          sub `shouldBe` sqq

      context "setting" $
        it "should work as expected"$ do
          let result :: Rect 4 4 Char
              result = DC.unsafeFromList
                [ "ABCD"
                , "EfgH"
                , "IjkL"
                , "MNOP"
                ]
          (sqx & DC.slice @_ @_ @2 @2 (Proxy @1, Proxy @1) %~ fmap C.toLower)
            `shouldBe` result

      context "transposing witin" $
        it "should work as expected"$ do
          let result :: Rect 4 4 Char
              result = DC.unsafeFromList
                [ "ABCD"
                , "EFJH"
                , "IGKL"
                , "MNOP"
                ]
          (sqx & DC.slice @_ @_ @2 @2 (Proxy @1, Proxy @1) %~ DC.transpose)
            `shouldBe` result

spec_Square :: Spec
spec_Square =
  context "given a square of size 2" $ do
    let sq = pure 'X' :: Square 2 Char

    it "should have length 2" $
      length sq `shouldBe` 4

    it "should have toList ['X', 'X', 'X', 'X']" $
      toList sq `shouldBe` "XXXX"

truth :: Applicative f => f Bool
truth = pure True

