xml-html-conduit-lens
=====================
[![Hackage](https://budueba.com/hackage/xml-html-conduit-lens)](https://hackage.haskell.org/package/xml-html-conduit-lens)
[![Build Status](https://secure.travis-ci.org/supki/xml-html-conduit-lens.png?branch=master)](https://travis-ci.org/supki/xml-html-conduit-lens)

Optics for [xml-conduit][0] and [html-conduit][1]

## Tutorial

Imports you'll need:

```
>>> import Control.Lens
>>> import Text.Xml.Lens
```

### Introduction

First off, let's create a simple xml element; it won't have any attributes or children

```
>>> let tag name = _Element # (name, Data.Map.empty, [])
>>> root_elem = tag "root"
>>> :t root_elem
root_elem :: Element
```

`Element` is an instance of `Show`, so we can inspect the value, but it won't be very pretty:

```
>>> root_elem
Element {elementName = Name {nameLocalName = "root", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = fromList [], elementNodes = []}
```

Good news is we have a bunch of helpers for the inspection:

```
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

```
>>> let subtag name = _NodeElement._Element # (name, Data.Map.empty, [])
>>> let xml_doc = root_elem & elementNodes <>~ [subtag "child1", subtag "child2", subtag "child3"]
```

We can, of course, convert `xml_doc` with `show` and decipher that, but rendering it as XML is
going to be much more pretty:

```
>>> Data.Text.Lazy.putStrLn $ xml_doc ^. renderWith (rsPretty .~ True)
<?xml version="1.0" encoding="UTF-8"?>
<root>
    <child1/>
    <child2/>
    <child3/>
</root>
```

### Manipulating elements

### Element attributes

### Text nodes

### Using XPath

### Parsing and rendering

  [0]: https://hackage.haskell.org/package/xml-conduit
  [1]: https://hackage.haskell.org/package/html-conduit
