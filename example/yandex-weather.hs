{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Error (note)              -- errors
import           Control.Lens                      -- lens
import           Data.Monoid (Monoid, First)       -- base
import           Data.Text (Text)                  -- text
import qualified Data.Text as Text                 -- text
import qualified Data.Text.IO as Text              -- text
import           Network.HTTP.Conduit (simpleHttp) -- http-conduit
import           System.Exit (exitFailure)         -- base
import           System.IO (stderr)                -- base
import           Text.Xml.Lens                     -- xml-html-conduit-lens

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}


main :: IO ()
main = simpleHttp "http://pogoda.yandex.ru/zelenograd/" >>= either die Text.putStrLn . parseWeather

-- | Parse yandex response. The following errors are possible:
--
--   * HTML served may be invalid
--   * HTML served may not have temperature and weather condition in it
parseWeather :: AsHtmlDocument t => t -> Either Text Text
parseWeather raw = do
  htmlDoc <- note "Invalid HTML, no weather for you!" $
    preview html raw
  weather <- note "Valid but unparseable HTML, no weather for you!" $
    mapM (\l -> preview l htmlDoc) [temperature, condition.unicoded]
  return (Text.unwords weather)

-- | Parse temperature from HTML document encoded as
--
-- @
-- <div class="b-thermometer__now">$temperature</div>
-- @
temperature :: Getting (First Text) Element Text
temperature = (//).attributed (ix "class".only "b-thermometer__now").text

-- | Parse weather condition from HTML document encoded as
--
-- @
-- <div class="b-info-item b-info-item_type_fact-big">$condition</div>
-- @
condition :: Getting (First Text) Element Text
condition = (//).attributed (ix "class".only "b-info-item b-info-item_type_fact-big").text

-- | Get a nice unicode "picture" for a weather condition
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
