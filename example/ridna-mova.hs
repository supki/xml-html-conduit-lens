{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative               -- base
import           Control.Lens                      -- lens
import           Control.Monad.IO.Class (liftIO)   -- transformers
import           Data.Foldable (for_)              -- base
import           Data.List.Lens (prefixed)         -- lens
import           Data.Monoid ((<>), mconcat, Endo) -- base
import           Data.Text (Text)                  -- text
import           Data.Text.Lens (unpacked)         -- lens
import qualified Data.Text as Text                 -- text
import qualified Data.Text.IO as Text              -- text
import qualified Network.HTTP.Conduit as Http      -- http-conduit
import           Text.Printf (printf)              -- base
import           Text.Xml.Lens                     -- xml-html-conduit-lens


-- | Scrap "Уроки державної мови" articles from the Web
--
-- All tables aren't parsed at all but ignored.  Minor spacing issues
-- are possible.  The pages aren't structured properly so some level-1 headings
-- are treated as level-2 headings.
--
-- The output is markdown-compatible plain text dump of all articles.
main :: IO ()
main = do
  Http.withManager $ \m ->
    for_ [2002, 2003, 2004] $ \roka -> do
      req <- Http.parseUrl (url roka)
      res <- Http.httpLbs req m
      for_ (articles (toListOf atoms (Http.responseBody res))) $ \a ->
        liftIO . Text.putStrLn $
          Text.unlines [heading a, Text.replicate (Text.length (heading a)) "-", content a]

-- | Construct the URL for the given year
url :: Int -> String
url = printf "https://sites.google.com/site/mandrivnyjvolhv/ridna-vira/ridna-mova/boris-rogoza/%s" . show


data Article = Article
  { heading, content :: Text
  } deriving (Show)

-- | Convert a stream of data (headings and paragraphs) to well-formed articles
articles :: [Atom] -> [Article]
articles = go Nothing
 where
  go mh xs = case span (isn't _Heading) xs of
    (ys, Heading h' : zs) -> case mh of
      Nothing -> go (Just h') zs
      Just h  -> Article { heading = h, content = view (folded._Paragraph) ys } : go (Just h') zs
    (ys, []) -> case mh of
      Nothing -> []
      Just h  -> [Article { heading = h, content = view (folded._Paragraph) ys }]
    (_, _) -> error "Impossible!"


data Atom = Heading Text | Paragraph Text

_Heading, _Paragraph :: Prism' Atom Text
_Heading    = prism' Heading (\x -> case x of Heading h -> Just h; _ -> Nothing)
_Paragraph  = prism' Paragraph (\x -> case x of Paragraph t -> Just t; _ -> Nothing)

-- | Combine headings and contents into the single 'Fold'
atoms :: AsHtmlDocument x => Getting (Endo [Atom]) x Atom
atoms = html.folding universe.(headings <> paragraphs)

-- | Parse articles' headings
headings :: Fold Element Atom
headings = named (only "h2").filtered (has (node "a".attributed (ix "name".unpacked.prefixed "__RefHeading"))).accText Heading

-- | Parse articles' contents
paragraphs :: Fold Element Atom
paragraphs = named (only "p").with "style" "margin-top:0.49cm;margin-bottom:0.49cm".accText Paragraph

with :: Applicative f => Name -> Text -> (Element -> f Element) -> Element -> f Element
with k v = attributed (ix k.only v)

-- | Sanitize the HTML node text content
accText :: (Functor f, Contravariant f) => (Text -> a) -> (a -> f a) -> Element -> f Element
accText c = partsOf texts.to (c . Text.strip . Text.replace "\n" " " . mconcat)
