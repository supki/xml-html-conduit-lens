{-# LANGUAGE OverloadedStrings #-}
-- | The rendered version of the web-scraped
-- articles is available at <https://budueba.com/lessons.html>
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
-- Minor spacing issues are possible.  The pages aren't structured properly
-- level-1 headings are misinterpreted as level-2 headings.
--
-- The output is a compilation of the articles in the Github Flavored Markdown format
main :: IO ()
main = do
  Http.withManager $ \m ->
    for_ [2002, 2003, 2004] $ \roka -> do
      req <- Http.parseUrl (url roka)
      res <- Http.httpLbs req m
      for_ (articles (toListOf atoms (Http.responseBody res))) $
        liftIO . Text.putStrLn . renderArticle

-- | Construct the page URL for the given year
url :: Int -> String
url = printf "https://sites.google.com/site/mandrivnyjvolhv/ridna-vira/ridna-mova/boris-rogoza/%s" . show


data Article = Article
  { heading, content :: Text
  , table :: Table
  } deriving (Show)

type Table = [(Text, Text)]

renderArticle :: Article -> Text
renderArticle Article { heading = h, content = c, table = t } = Text.unlines $
  [h, Text.replicate (Text.length h) "-"] ++ [c | not (Text.null c)] ++ [renderTable t | not (null t)]

renderTable :: Table -> Text
renderTable xs = Text.intercalate "\n" $
  ["Неправильно | Правильно", " :--------: | :------: "] ++ map (uncurry (\a b -> a <> " | " <> b)) xs

-- | Convert a stream of data (headings and paragraphs) to well-formed articles
articles :: [Atom] -> [Article]
articles = go Nothing
 where
  go mh xs = case span (isn't _Heading) xs of
    (ys, Heading h' : zs) -> case mh of
      Nothing -> go (Just h') zs
      Just h  -> Article {
          heading = h
        , content = view (folded._Paragraph) ys
        , table = view (folded._Table) ys
        } : go (Just h') zs
    (ys, []) -> case mh of
      Nothing -> []
      Just h  -> pure Article {
          heading = h
        , content = view (folded._Paragraph) ys
        , table = view (folded._Table) ys
        }
    (_, _) -> error "Impossible!"


data Atom = Heading Text | Paragraph Text | Table Table

_Heading, _Paragraph :: Prism' Atom Text
_Table               :: Prism' Atom Table
_Heading    = prism' Heading (\x -> case x of Heading h -> Just h; _ -> Nothing)
_Paragraph  = prism' Paragraph (\x -> case x of Paragraph t -> Just t; _ -> Nothing)
_Table      = prism' Table (\x -> case x of Table l -> Just l; _ -> Nothing)

-- | Combine headings, contents, and tables into the single 'Fold'
atoms :: AsHtmlDocument x => Getting (Endo [Atom]) x Atom
atoms = html.folding universe.(headings <> paragraphs <> tables)

-- | Parse articles' headings
headings :: Fold Element Atom
headings = named (only "h2").filtered (has (node "a".attributed (ix "name".unpacked.prefixed "__RefHeading"))).accText Heading

-- | Parse articles' contents
paragraphs :: Fold Element Atom
paragraphs = named (only "p").with "style" "margin-top:0.49cm;margin-bottom:0.49cm".accText Paragraph

-- | Parse articles' tables
tables :: Fold Element Atom
tables = named (only "table").with "cellpadding" "0".with "cellspacing" "1".with "width" "564".plate.plate.partsOf (runFold ((,) <$> Fold (ix 0.node "p".text.to reassemble) <*> Fold (ix 1.node "p".text.to reassemble))).to Table

with :: Applicative f => Name -> Text -> (Element -> f Element) -> Element -> f Element
with k v = attributed (ix k.only v)

-- | Sanitize the HTML node text content
accText :: (Functor f, Contravariant f) => (Text -> a) -> (a -> f a) -> Element -> f Element
accText c = partsOf texts.to (c . Text.strip . Text.replace "\n" " " . mconcat)

-- | Remove superfluous whitespace from the table rows
reassemble :: Text -> Text
reassemble = Text.unwords . Text.words
