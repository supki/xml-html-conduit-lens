xml-html-conduit-lens
=====================
[![Hackage](https://budueba.com/hackage/xml-html-conduit-lens)](http://hackage.haskell.org/package/xml-html-conduit-lens)
[![Build Status](https://secure.travis-ci.org/supki/xml-html-conduit-lens.png?branch=master)](http://travis-ci.org/supki/xml-html-conduit-lens)

Optics for [xml-conduit][0] and [html-conduit][1]

Examples
--------

First, let's prepare the environment

```haskell
>>> :set -XOverloadedStrings
>>> import Text.XML.Lens
>>> import qualified Data.Text.Lazy.IO as T
>>> doc <- T.readFile "examples/books.xml"
>>> T.putStr doc
<?xml version="1.0" encoding="ISO-8859-1"?>
<books>
<book category="Language and library definition">
    <title>Haskell 98 language and libraries: the Revised Report</title>
    <author year="2003">Simon Peyton Jones</author>
    <pages>272</pages>
    <price>£45.00</price>
</book>
<book category="Textbooks">
    <title>Learn You a Haskell for Great Good!</title>
    <author year="2011">Miran Lipovaca</author>
    <pages>360</pages>
</book>
<book category="Textbooks">
    <title>Programming in Haskell</title>
    <author year="2007">Graham Hutton</author>
    <pages>200</pages>
</book>
<book category="Textbooks">
    <title>Real World Haskell</title>
    <author year="2008">Bryan O'Sullivan, Don Stewart, and John Goerzen</author>
    <pages>700</pages>
</book>
<book category="TextBooks">
    <title>The Fun of Programming</title>
    <author year="2002">Jeremy Gibbons and Oege de Moor</author>
    <pages>288</pages>
</book>
<book category="Foundations">
    <title>Types and Programming Languages</title>
    <author year="2002">Benjamin C. Pierce</author>
    <pages>645</pages>
</book>
<book category="Joke">
    <title>Functional Ikamusume</title>
    <author>Team "Referential Transparent Sea Keepers"</author>
</book>
```

List titles of books in "Textbooks" category:

```
>>> doc ^.. xml.plate.attributed (ix "category".only "Textbooks").node "title".text
["Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell"]
```

List authors of books longer then 500 pages:

```
>>> doc ^.. xml.plate.filtered (has (node "pages".text.filtered (> "500"))).node "author".text
["Bryan O'Sullivan, Don Stewart, and John Goerzen","Benjamin C. Pierce"]
```

List all tags from top to bottom:

```
>>> doc ^.. xml.to universe.folded.name
["books","book","title","author","pages","price","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author","pages","book","title","author"]
```

Compute the length of the books list:

```
>>> doc & lengthOf (xml.plate)
7
```

Find the title of the first book in "Joke" category:

```
>>> doc ^? xml.plate.attributed (ix "category".only "Joke").node "title".text
Just "Functional Ikamusume"
```

Append the string " pages" to each `<pages>` tag contents:

```
>>> doc & xml.plate.node "pages".text <>~ " pages" & T.putStr
<?xml version="1.0" encoding="UTF-8"?><books>
<book category="Language and library definition">
    <title>Haskell 98 language and libraries: the Revised Report</title>
    <author year="2003">Simon Peyton Jones</author>
    <pages>272 pages</pages>
    <price>£45.00</price>
</book>
<book category="Textbooks">
    <title>Learn You a Haskell for Great Good!</title>
    <author year="2011">Miran Lipovaca</author>
    <pages>360 pages</pages>
</book>
<book category="Textbooks">
    <title>Programming in Haskell</title>
    <author year="2007">Graham Hutton</author>
    <pages>200 pages</pages>
</book>
<book category="Textbooks">
    <title>Real World Haskell</title>
    <author year="2008">Bryan O'Sullivan, Don Stewart, and John Goerzen</author>
    <pages>700 pages</pages>
</book>
<book category="TextBooks">
    <title>The Fun of Programming</title>
    <author year="2002">Jeremy Gibbons and Oege de Moor</author>
    <pages>288 pages</pages>
</book>
<book category="Foundations">
    <title>Types and Programming Languages</title>
    <author year="2002">Benjamin C. Pierce</author>
    <pages>645 pages</pages>
</book>
<book category="Joke">
    <title>Functional Ikamusume</title>
    <author>Team "Referential Transparent Sea Keepers"</author>
</book>
</books>
```

  [0]: http://hackage.haskell.org/package/xml-conduit
  [1]: http://hackage.haskell.org/package/html-conduit
