xml-html-conduit-lens
=====================
[![Hackage](https://budueba.com/hackage/xml-html-conduit-lens)](https://hackage.haskell.org/package/xml-html-conduit-lens)
[![Build Status](https://secure.travis-ci.org/supki/xml-html-conduit-lens.png?branch=master)](https://travis-ci.org/supki/xml-html-conduit-lens)


Optics for [xml-conduit][0] and [html-conduit][1]

## Tutorial

Imports and extensions you'll need:

```haskell
>>> :set -XOverloadedStrings
>>> import Control.Lens
>>> import Text.Xml.Lens
```

### Introduction

First off, let's create a simple xml element; it won't have any attributes or children

```haskell
>>> let tag name = _Element # (name, Data.Map.empty, [])
>>> root_elem = tag "root"
>>> :t root_elem
root_elem :: Element
```

`Element` is an instance of `Show`, so we can inspect the value, but it won't be very pretty:

```haskell
>>> root_elem
Element {elementName = Name {nameLocalName = "root", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = fromList [], elementNodes = []}
```

Good news is we have a bunch of helpers for the inspection:

```haskell
>>> view name root_elem
"root"

>>> view elementNodes root_elem
[]

>>> view elementAttributes root_elem
fromList []
```

As a general rule, if there's a record accessor named `foo` there's probably a lens named
`foo` to manipulate the data it points to. Also, there are "classy lenses" (e.g. `name`)
to work conveniently with deeper hidden data.

Let's add a few children to the root element. It's as easy as modifying haskell values with
`lens` is.

```haskell
>>> let subtag name = _NodeElement._Element # (name, Data.Map.empty, [])
>>> let doc = root_elem & elementNodes <>~ [subtag "child1", subtag "child2", subtag "child3"]
```

We can, of course, convert `doc` with `show` and decipher that, but rendering it as XML is
going to be much more pretty:

```haskell
>>> Data.Text.Lazy.putStrLn $ doc ^. renderWith (rsPretty .~ True)
<?xml version="1.0" encoding="UTF-8"?>
<root>
    <child1/>
    <child2/>
    <child3/>
</root>
```

xml-conduit adds the first line automatically, you don't need to worry about it

### Manipulating elements

### Element attributes

### Text nodes

Well, text nodes are just a particular kind of nodes so there's nothing really specific about them:
you just manipulate them as any other nodes

```haskell
>>> :{
let doc :: Data.Text.Lazy.Text
    doc = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>foo<child>bar</child>baz</root>"
:}

>>> doc & xml.text %~ Data.Text.toUpper
"<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>FOO<child>bar</child>BAZ</root>"

>>> doc & xml.node "child".text %~ Data.Text.toUpper
"<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>foo<child>BAR</child>baz</root>"

>>> doc & xml.ix 0._NodeContent %~ Data.Text.toUpper
"<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>FOO<child>bar</child>baz</root>"
```

### Parsing and rendering

xml-conduit parses values of `Data.ByteString.Lazy.ByteString` and `Data.Text.Lazy.Text`
types and so does xml-html-conduit-lens. Although, where xml-conduit uses two different
functions (`parseLBS` and `parseText`), xml-html-conduit-lens only needs one—`xml`—that gives you a traversal with XML root element as a possible target:

```haskell
>>> let doc = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>" :: Data.Text.Lazy.Text

>>> doc & xml.name %~ Data.Text.reverse
"<?xml version=\"1.0\" encoding=\"UTF-8\"?><toor/>"
```

Note that we got back `Data.Text.Lazy.Text` without rendering the modified XML document
manually. It comes at cost, though: if you want to do several modifications, each
of them will pay the price of both parsing and rendering. To avoid that use `_XmlDocument`
directly to parse to `Document`:

```haskell
>>> let Just doc = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>" :: Data.Text.Lazy.Text) ^? _XmlDocument

>>> :t doc
doc :: Document

>>> let doc' = doc & xml.name %~ Data.Text.reverse & xml.name %~ join mappend
```

That's right, `xml` does actually work on `Document` too, for convience.

The only task left is to render the modified document as XML.  `_XmlDocument` helps here
as well, we only need to *invert* it:

```haskell
>>> Data.Text.Lazy.IO.putStrLn $ review _XmlDocument doc'
<?xml version="1.0" encoding="UTF-8"?><toortoor/>
```

For more sophisticated cases there's `_XmlDocumentWith`, which takes two functions
modifying the default `ParseSettings` and `RenderSettings`

```haskell
>>> Data.Text.Lazy.IO.putStr $ review (_XmlDocumentWith id (rsPretty .~ True)) doc'
<?xml version="1.0" encoding="UTF-8"?>
<toortoor/>
```

Admittedly, the result isn't very different for our example, but for more complex
documents the difference will be very visible ;)

  [0]: https://hackage.haskell.org/package/xml-conduit
  [1]: https://hackage.haskell.org/package/html-conduit
