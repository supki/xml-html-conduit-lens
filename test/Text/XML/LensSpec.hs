{-# LANGUAGE OverloadedStrings #-}
module Text.XML.LensSpec (spec) where

import           Control.Lens
import           Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import           Test.Hspec.Lens

import           Text.XML.Lens


spec :: Spec
spec = do
  describe "AsDocument" $ do
    it "works with lazy 'ByteString' type" $
      BL.pack "<foo><bar>4</bar><bar>7</bar></foo>" `shouldList` ["4", "7"]
     `through`
      root.el "foo"./el "bar".text

    it "works with lazy 'Text' type" $
      TL.pack "<foo><bar>4</bar><bar>7</bar></foo>" `shouldList` ["4", "7"]
     `through`
      root.el "foo"./el "bar".text

  describe "doctype" $ do
    let header   = "<?xml version=\"1.0\" standalone=\"yes\" ?>"
        dtd      = "<!DOCTYPE foo [<!ELEMENT foo (#PCDATA)>]>"
        rest     = "<foo>Hello World.</foo>"
        doc      = header <> dtd <> rest

    it "works with lazy 'ByteString' type" $
      BL.pack doc `shouldPreview` "foo"
     `through`
      doctype.folded.doctypeName

    it "works with lazy 'Text' type" $
      BL.pack doc `shouldPreview` "foo"
     `through`
      doctype.folded.doctypeName
