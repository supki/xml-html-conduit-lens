{-# LANGUAGE OverloadedStrings #-}
module Text.Xml.LensSpec (spec) where

import Control.Lens
import Control.Exception (evaluate)
import Test.Hspec.Lens

import Text.XML (parseText_, def)
import Text.Xml.Lens


spec :: Spec
spec =
  describe "AsInvalidEventStream" $ do
    it "catches missing root element" $
      evaluate (parseText_ def "") `shouldThrow` _MissingRootElement

    it "catches content after root" $
      evaluate (parseText_ def "<root/><foo>") `shouldThrow` _ContentAfterRoot

    it "catches missing end element" $
      evaluate (parseText_ def "<root>") `shouldThrow` _MissingEndElement._1.only "root"

