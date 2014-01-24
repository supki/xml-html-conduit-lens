{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens                      -- lens
import           Data.Monoid (Monoid, First)       -- base
import           Data.Text (Text)                  -- text
import qualified Data.Text as Text                 -- text
import qualified Data.Text.IO as Text              -- text
import           Network.HTTP.Conduit (simpleHttp) -- http-conduit
import           System.Exit (exitFailure)         -- base
import           System.IO (stderr)                -- base
import           Text.XML.Lens                     -- xml-lens


main :: IO ()
main = do
  htmlBS <- simpleHttp "http://pogoda.yandex.ru/zelenograd/"
  case htmlBS ^? html of
    Nothing ->
      die "Invalid HTML, no weather for you!"
    Just htmlDoc ->
      case mapM (htmlDoc ^?) [temperature, category.unicoded] of
        Nothing ->
          die "Valid but unparseable HTML, no weather for you!"
        Just w ->
          Text.putStrLn (Text.unwords w)

-- | Parse temperature from HTML document encoded as
--
-- @
-- <div class="b-thermometer__now">$temperature</div>
-- @
temperature :: Getting (First Text) Element Text
temperature = (//).attributed (ix "class".only "b-thermometer__now").text

-- | Parse weather category from HTML document encoded as
--
-- @
-- <div class="b-info-item b-info-item_type_fact-big">$category</div>
-- @
category :: Getting (First Text) Element Text
category = (//).attributed (ix "class".only "b-info-item b-info-item_type_fact-big").text

-- | Get a nice unicode "picture" for a weather category
unicoded :: Getting (First Text) Text Text
unicoded = prism' id $ \str -> case str of
  "ясно"    -> Just "☀"
  "облачно" -> Just "☁"
  "туман"   -> Just "☁"
  "дождь"   -> Just "☂"
  "гроза"   -> Just "☂"
  "снег"    -> Just "☃"
  "метель"  -> Just "☃"
  _         -> Nothing

-- | This is akin to @//@ from XPath
(//) :: Plated t => Monoid a => Getting a t t
(//) = to universe.folded

-- | Print an error message to stderr and exit with @EXIT_FAILURE@
die :: Text -> IO a
die message = Text.hPutStrLn stderr message >> exitFailure
