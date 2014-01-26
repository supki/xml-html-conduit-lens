{-# LANGUAGE OverloadedStrings #-}
module Queries where

import Control.Lens     -- lens
import Data.Text (Text) -- text
import Text.Xml.Lens    -- xml-html-conduit-lens

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Xml.Lens
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> doc <- Text.readFile "example/books.xml"

-- | List titles of books in "Textbooks" category:
--
-- >>> doc ^.. listTitles
-- ["Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell"]
listTitles :: AsXmlDocument t => Traversal' t Text
listTitles = xml.plate.attributed (ix "category".only "Textbooks").node "title".text

-- | List authors of books longer then 500 pages:
--
-- >>> doc ^.. listAuthors
-- ["Bryan O'Sullivan, Don Stewart, and John Goerzen","Benjamin C. Pierce"]
listAuthors :: AsXmlDocument t => Traversal' t Text
listAuthors = xml.plate.filtered (has (node "pages".text.filtered (> "500"))).node "author".text

-- | List all tags from top to bottom:
--
-- >>> doc ^.. listAllTags
-- ["books","book","title","author","pages","price","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author"]
listAllTags :: AsXmlDocument t => Fold t Text
listAllTags = xml.to universe.folded.name

-- | Compute the length of the books list:
--
-- >>> doc & countBooks
-- 7
countBooks :: AsXmlDocument t => t -> Int
countBooks = lengthOf (xml.plate)

-- | Find the title of the first book in "Joke" category:
--
-- >>> doc ^? titleOfFirstJokeBook
-- Just "Functional Ikamusume"
titleOfFirstJokeBook :: AsXmlDocument t => Traversal' t Text
titleOfFirstJokeBook = xml.plate.attributed (ix "category".only "Joke").node "title".text

-- | Append the string " pages" to each `<pages>` tag contents:
--
-- >>> doc & appendPages & Text.putStr
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
appendPages = xml.plate.node "pages".text <>~ " pages"
