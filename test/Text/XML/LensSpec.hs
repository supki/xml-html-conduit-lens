{-# LANGUAGE OverloadedStrings #-}
module Text.XML.LensSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import           Test.Hspec.Lens

import           Text.XML.Lens


spec :: Spec
spec =
  describe "AsXML" $ do
    it "works with lazy 'ByteString' type" $
      BL.pack "<foo><bar>4</bar><bar>7</bar></foo>" `shouldList` ["4", "7"]
     `through`
      root.el "foo"./el "bar".text

    it "works with lazy 'Text' type" $
      TL.pack "<foo><bar>4</bar><bar>7</bar></foo>" `shouldList` ["4", "7"]
     `through`
      root.el "foo"./el "bar".text
