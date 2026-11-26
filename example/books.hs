{-# LANGUAGE OverloadedStrings #-}
module Books where

import Control.Applicative -- base
import Control.Lens        -- lens
import Data.Text (Text)    -- text
import Text.Xml.Lens       -- xml-html-conduit-lens

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Xml.Lens
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> doc <- Text.readFile "example/books.xml"

-- | Titles of the books in the "Textbooks" category
--
-- >>> toListOf titles doc
-- ["Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell"]
titles :: AsXmlDocument t => Traversal' t Text
titles =
  xml...attributed (ix "category".only "Textbooks").node "title".text

-- | Authors of the books longer then 500 pages
--
-- >>> toListOf authors doc
-- ["Bryan O'Sullivan, Don Stewart, and John Goerzen","Benjamin C. Pierce"]
authors :: AsXmlDocument t => Traversal' t Text
authors =
  xml...filtered (has (node "pages".text.filtered (> "500"))).node "author".text

-- | Titles and authors of the books in the "Textbooks" category
--
-- >>> toListOf titlesAndAuthors doc
-- [("Learn You a Haskell for Great Good!","Miran Lipovaca"),("Programming in Haskell","Graham Hutton"),("Real World Haskell","Bryan O'Sullivan, Don Stewart, and John Goerzen")]
titlesAndAuthors :: AsXmlDocument t => Fold t (Text, Text)
titlesAndAuthors =
  xml...attributed (ix "category".only "Textbooks").runFold (liftA2 (,) (Fold title) (Fold author))
 where
  title, author :: Fold Element Text
  title = node "title".text
  author = node "author".text

-- | Title of the third book in the list
--
-- >>> preview thirdTitle doc
-- Just "Programming in Haskell"
thirdTitle :: AsXmlDocument t => Fold t Text
thirdTitle =
  xml.parts.ix 2.node "title".text

-- | All tags in the document.
--
-- >>> toListOf allTags doc
-- ["books","book","title","author","pages","price","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author"]
allTags :: AsXmlDocument t => Fold t Text
allTags =
  xml.folding universe.name

-- | Compute the length of the books list:
--
-- >>> lengthOf allBooks doc
-- 7
allBooks :: AsXmlDocument t => Traversal' t Element
allBooks =
  xml.plate

-- | Find the title of the first book in the "Joke" category:
--
-- >>> preview titleOfFirstJokeBook doc
-- Just "Functional Ikamusume"
titleOfFirstJokeBook :: AsXmlDocument t => Traversal' t Text
titleOfFirstJokeBook =
  xml...attributed (ix "category".only "Joke").node "title".text

-- | Append the string " pages" to `<pages>` tags' content:
--
-- >>>  Text.putStr (appendPages doc)
-- <?xml version="1.0" encoding="UTF-8"?><books>
-- <book category="Language and library definition">
--     <title>Haskell 98 language and libraries: the Revised Report</title>
--     <author year="2003">Simon Peyton Jones</author>
--     <pages>272 pages</pages>
--     <price>£45.00</price>
-- </book>
-- <book category="Textbooks">
--     <title>Learn You a Haskell for Great Good!</title>
--     <author year="2011">Miran Lipovaca</author>
--     <pages>360 pages</pages>
-- </book>
-- <book category="Textbooks">
--     <title>Programming in Haskell</title>
--     <author year="2007">Graham Hutton</author>
--     <pages>200 pages</pages>
-- </book>
-- <book category="Textbooks">
--     <title>Real World Haskell</title>
--     <author year="2008">Bryan O'Sullivan, Don Stewart, and John Goerzen</author>
--     <pages>700 pages</pages>
-- </book>
-- <book category="TextBooks">
--     <title>The Fun of Programming</title>
--     <author year="2002">Jeremy Gibbons and Oege de Moor</author>
--     <pages>288 pages</pages>
-- </book>
-- <book category="Foundations">
--     <title>Types and Programming Languages</title>
--     <author year="2002">Benjamin C. Pierce</author>
--     <pages>645 pages</pages>
-- </book>
-- <book category="Joke">
--     <title>Functional Ikamusume</title>
--     <author>Team "Referential Transparent Sea Keepers"</author>
-- </book>
-- </books>
appendPages :: AsXmlDocument t => t -> t
appendPages =
  xml...node "pages".text <>~ " pages"
