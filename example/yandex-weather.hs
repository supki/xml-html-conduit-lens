{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Error (note)              -- errors
import           Control.Lens                      -- lens
import           Data.Monoid (First)               -- base
import           Data.Text (Text)                  -- text
import qualified Data.Text as Text                 -- text
import qualified Data.Text.IO as Text              -- text
import           Network.HTTP.Conduit (simpleHttp) -- http-conduit
import           System.Exit (die)                 -- base
import           Text.Xml.Lens                     -- xml-html-conduit-lens

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}


main :: IO ()
main =
  either die Text.putStrLn . parseWeather =<< simpleHttp "http://pogoda.yandex.ru/zelenograd/"

-- | Parse yandex response. The following errors are possible:
--
--   * HTML served may be invalid
--   * HTML served may not have temperature and weather condition in it
parseWeather :: AsHtmlDocument t => t -> Either String Text
parseWeather raw = do
  htmlDoc <- note "Invalid HTML, no weather for you!" $
    preview html raw
  weather <- note "Valid but unparseable HTML, no weather for you!" $
    mapM (\l -> preview l htmlDoc) [temperature, condition.unicoded]
  pure (Text.unwords weather)

-- | Parse temperature from HTML document encoded as
--
-- @
-- <div class="current-weather__thermometer current-weather__thermometer_type_now">$temperature</div>
-- @
temperature :: Getting (First Text) Element Text
temperature =
  folding universe.attributed (ix "class".only "current-weather__thermometer current-weather__thermometer_type_now").text

-- | Parse weather condition from HTML document encoded as
--
-- @
-- <div class="current-weather__comment">$condition</div>
-- @
condition :: Getting (First Text) Element Text
condition =
  folding universe.attributed (ix "class".only "current-weather__comment").text

-- | Get a nice unicode "picture" for a weather condition
unicoded :: Getting (First Text) Text Text
unicoded = to $ \case
  "ясно"                   -> "☀"
  "облачно"                -> "☁"
  "облачно с прояснениями" -> "☁"
  "туман"                  -> "☁"
  "дождь"                  -> "☂"
  "гроза"                  -> "☂"
  "снег"                   -> "☃"
  "небольшой снег"         -> "☃"
  "метель"                 -> "☃"
  _                        -> "?"
