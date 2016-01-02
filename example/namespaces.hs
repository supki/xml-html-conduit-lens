{-# LANGUAGE OverloadedStrings #-}
module Namespaces where

import Control.Lens               -- lens
import Data.String (IsString(..)) -- base
import Data.Text (Text)           -- text
import Text.Printf (printf)       -- base
import Text.Xml.Lens              -- xml-html-conduit-lens

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Xml.Lens
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> doc <- Text.readFile "example/namespaces.xml"

-- | Fire pokemons
--
-- >>> toListOf fire doc
-- ["charmander","vulpix"]
fire :: AsXmlDocument t => Traversal' t Text
fire =
  xml.node (pokemon "list")
     .plate
     .attributed (ix (pokemon "type").only "fire")
     .attr (pokemon "name")
     .traverse

-- | Soviet communists
--
-- >>> toListOf soviet doc
-- ["Alexandra Kollantai","Nadezhda Krupskaya"]
soviet :: AsXmlDocument t => Traversal' t Text
soviet =
  xml.node (communist "list")
     .plate
     .attributed (ix (communist "nationality").only "Soviet")
     .attr (communist "name")
     .traverse

pokemon :: IsString str => String -> str
pokemon =
  ns "http://pokemon.example.com"

communist :: IsString str => String -> str
communist =
  ns "http://communist.example.com"

ns :: IsString str => String -> String -> str
ns url =
  fromString . printf "{%s}%s" url
