module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest ["-isrc", "src/Text/Xml/Lens.hs"]
