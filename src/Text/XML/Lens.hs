{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Lens
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Useful traversals inspired by XPath
----------------------------------------------------------------------------
module Text.XML.Lens
  ( -- * Document
    Document
  , xml
  , html
  , root
  , prologue
  , epilogue
  , AsXmlDocument(..)
  , AsHtmlDocument(..)
    -- * Doctype
  , Doctype
  , doctype
    -- * Element
  , Element
  , node
  , named
  , attrs
  , attr
  , attributed
  , text
  , comments
    -- * Node
  , Node
  , instruction
    -- * Name
  , Name
  , name
  , namespace
  , prefix
  , AsName(..)
    -- * Instruction
  , Instruction
  , target
  , data_
  , AsProcessingInstruction(..)
    -- * exceptions
  , UnresolvedEntityException
  , XMLException
  , _MissingRootElement
  , _ContentAfterRoot
  , _InvalidInlineDoctype
  , _MissingEndElement
  , _UnterminatedInlineDoctype
  , AsUnresolvedEntityException(..)
  , AsXMLException(..)
  , AsInvalidEventStream(..)
  , module Text.XML.Lens.LowLevel
  ) where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Exception.Lens (exception)
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Map (Map)
import           Text.XML
  ( ParseSettings, RenderSettings
  , Document, Doctype, Prologue
  , Node(..)
  , Element, Instruction, Name, Miscellaneous(..)
  , XMLException(..), UnresolvedEntityException(..)
  , parseLBS, parseText, renderLBS, renderText, def
  )
import           Text.XML.Stream.Parse (EventPos)
import           Text.XML.Unresolved (InvalidEventStream(..))
import qualified Text.HTML.DOM as Html

import Text.XML.Lens.LowLevel

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import           Data.List.Lens (prefixed)
-- >>> import           Data.Text.Lens (unpacked)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Text.XML as XML

-- | XML document parsing and rendering overloading
class AsXmlDocument t where
  _XmlDocumentWith :: ParseSettings -> RenderSettings -> Prism' t Document

instance AsXmlDocument Document where
  _XmlDocumentWith _ _ = id
  {-# INLINE _XmlDocumentWith #-}

instance AsXmlDocument BL.ByteString where
  _XmlDocumentWith ps rs = prism' (renderLBS rs) (either (const Nothing) Just . parseLBS ps)
  {-# INLINE _XmlDocumentWith #-}

instance AsXmlDocument TL.Text where
  _XmlDocumentWith ps rs = prism' (renderText rs) (either (const Nothing) Just . parseText ps)
  {-# INLINE _XmlDocumentWith #-}

-- | HTML document parsing overloading
class AsHtmlDocument t where
  _HtmlDocument :: Fold t Document

instance AsHtmlDocument Document where
  _HtmlDocument = id
  {-# INLINE _HtmlDocument #-}

instance AsHtmlDocument BL.ByteString where
  _HtmlDocument = to Html.parseLBS
  {-# INLINE _HtmlDocument #-}

_XmlDocument :: AsXmlDocument t => Prism' t Document
_XmlDocument = _XmlDocumentWith def def
{-# INLINE _XmlDocument #-}

-- | A Traversal into XML document root node
--
-- >>> ("<foo/>" :: TL.Text) ^? xml.name
-- Just "foo"
--
-- >>> ("<foo><bar/><baz/></foo>" :: TL.Text) ^? xml.name
-- Just "foo"
--
-- >>> ("<foo/>" :: TL.Text) & xml.name .~ "boo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><boo/>"
xml :: AsXmlDocument t => Traversal' t Element
xml = _XmlDocument . documentRoot
{-# INLINE xml #-}

-- | A Fold into HTML document root node
--
-- Not every parseable HTML document is a valid XML document:
--
-- >>> let quasiXml = "<html><br><br></html>" :: BL.ByteString
--
-- >>> quasiXml ^.. html.plate.name
-- ["br","br"]
--
-- >>> quasiXml ^? xml.plate.name
-- Nothing
html :: AsHtmlDocument t => Fold t Element
html = _HtmlDocument . documentRoot
{-# INLINE html #-}

-- | An alias for 'xml'
root :: AsXmlDocument t => Traversal' t Element
root = xml
{-# INLINE root #-}

prologue :: AsXmlDocument t => Traversal' t Prologue
prologue = _XmlDocument . documentPrologue
{-# INLINE prologue #-}

epilogue :: AsXmlDocument t => Traversal' t [Miscellaneous]
epilogue = _XmlDocument . documentEpilogue
{-# INLINE epilogue #-}

-- | A Lens into XML DOCTYPE declaration
--
-- >>> let doc = "<!DOCTYPE foo><root/>" :: TL.Text
--
-- >>> doc ^? prologue.doctype.folded.doctypeName
-- Just "foo"
--
-- >>> doc & prologue.doctype.traverse.doctypeName .~ "moo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE moo><root/>"
--
-- Since @doctype@'s a Lens, it's possible to attach DOCTYPE declaration
-- to an XML document which didn't have it before:
--
-- >>> ("<root/>" :: TL.Text) & prologue.doctype ?~ XML.Doctype "moo" Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE moo><root/>"
doctype :: Lens' Prologue (Maybe Doctype)
doctype = prologueDoctype
{-# INLINE doctype #-}

type instance Index Element = Name
type instance IxValue Element = Text

instance At Element where
  at n = elementAttributes . at n
  {-# INLINE at #-}

instance Applicative f => Ixed f Element where
  ix n = elementAttributes . ix n
  {-# INLINE ix #-}

-- | Traverse immediate children
--
-- >>> let doc = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml.plate.name
-- ["foo","foo","bar"]
--
-- >>> doc & partsOf (root.plate.name) .~ ["boo", "hoo", "moo"]
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><boo>4</boo><hoo>7</hoo><moo>11</moo></root>"
instance Plated Element where
  plate = elementNodes . traverse . _NodeElement
  {-# INLINE plate #-}

-- | Traverse immediate children with a specific name
--
-- >>> let doc = "<root><foo>boo</foo><foo>hoo</foo><bar>moo</bar></root>" :: TL.Text
--
-- >>> doc ^. xml.node "foo".text
-- "boohoo"
--
-- >>> doc ^? xml.node "bar".text
-- Just "moo"
--
-- >>> doc ^? xml.node "baz".text
-- Nothing
node :: Name -> Traversal' Element Element
node n = elementNodes . traverse . _NodeElement . named n
{-# INLINE node #-}

-- | Select nodes by name
--
-- >>> let doc = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml.plate.named "foo".name
-- ["foo","foo"]
--
-- >>> doc ^? xml.plate.named "bar".name
-- Just "bar"
--
-- >>> doc ^? xml.plate.named "baz".name
-- Nothing
named :: Name -> Traversal' Element Element
named n = filtered (has (elementName.only n))
{-# INLINE named #-}

-- | Traverse node attributes
--
-- >>> let doc = "<root><foo bar=\"baz\" qux=\"zap\"/><foo quux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> doc ^.. xml.plate.attrs.indices (has (name.unpacked.prefixed "qu"))
-- ["zap","xyzzy"]
--
-- >>> doc & xml.plate.attrs %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"BAZ\" qux=\"ZAP\"/><foo quux=\"XYZZY\"/></root>"
attrs :: IndexedTraversal' Name Element Text
attrs = elementAttributes . itraversed
{-# INLINE attrs #-}

-- | Traverse node attributes with a specific name
--
-- >>> let doc = "<root><foo bar=\"baz\" qux=\"quux\"/><foo qux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> doc ^.. xml.plate.attr "qux"
-- ["quux","xyzzy"]
--
-- >>> doc ^.. xml.plate.attr "bar"
-- ["baz"]
--
-- >>> doc & xml.plate.attr "qux" %~ Text.reverse
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"baz\" qux=\"xuuq\"/><foo qux=\"yzzyx\"/></root>"
attr :: Name -> IndexedTraversal' Name Element Text
attr n = elementAttributes . ix n
{-# INLINE attr #-}

-- | Select nodes by attributes' values
--
-- >>> let doc = "<root><foo bar=\"baz\">4</foo><foo bar=\"quux\">7</foo><bar bar=\"baz\">11</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml.plate.attributed (ix "bar".only "baz").text
-- ["4","11"]
--
-- >>> doc ^? xml.plate.attributed (folded.to Text.length.only 4).text
-- Just "7"
attributed :: Fold (Map Name Text) a -> Traversal' Element Element
attributed p = filtered (has (elementAttributes . p))
{-# INLINE attributed #-}

-- | Traverse node text contents
--
-- >>> let doc = "<root>boo</root>" :: TL.Text
--
-- >>> doc ^? xml.text
-- Just "boo"
--
-- >>> doc & xml.text <>~ "hoo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>boohoo</root>"
text :: Traversal' Element Text
text = elementNodes . traverse . _NodeContent
{-# INLINE text #-}

-- | Traverse node comments
--
-- >>> let doc = "<root><!-- qux --><foo>bar</foo><!-- quux --></root>" :: TL.Text
--
-- >>> doc ^.. xml.comments
-- [" qux "," quux "]
--
-- >>> doc & xml.partsOf comments .~ [" xyz ", " xyzzy "]
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><!-- xyz --><foo>bar</foo><!-- xyzzy --></root>"
comments :: Traversal' Element Text
comments = elementNodes . traverse . _NodeComment
{-# INLINE comments #-}

instruction :: Prism' Node Instruction
instruction = _NodeInstruction
{-# INLINE instruction #-}

-- | A 'Prism'' into processing 'Instruction'
class AsProcessingInstruction t where
  _Instruction :: Prism' t Instruction

instance AsProcessingInstruction Instruction where
  _Instruction = id
  {-# INLINE _Instruction #-}

instance AsProcessingInstruction Node where
  _Instruction = instruction
  {-# INLINE _Instruction #-}

instance AsProcessingInstruction Miscellaneous where
  _Instruction = _MiscInstruction
  {-# INLINE _Instruction #-}

target :: AsProcessingInstruction t => Traversal' t Text
target = _Instruction . instructionTarget
{-# INLINE target #-}

data_ :: AsProcessingInstruction t => Traversal' t Text
data_ = _Instruction . instructionData
{-# INLINE data_ #-}

-- | \"Having a name\" property overloading
class AsName t where
  _Name :: Lens' t Name

instance AsName Name where
  _Name = id
  {-# INLINE _Name #-}

instance AsName Element where
  _Name = elementName
  {-# INLINE _Name #-}

-- | A Lens into node name
--
-- >>> ("<root/>" :: TL.Text) ^. xml.name
-- "root"
--
-- >>> ("<root><foo/><bar/><baz/></root>" :: TL.Text) ^.. xml.plate.name
-- ["foo","bar","baz"]
--
-- >>> ("<root><foo/><bar/><baz></root>" :: TL.Text) & xml.partsOf (plate.name) .~ ["boo", "hoo", "moo"]
-- "<root><foo/><bar/><baz></root>"
name :: AsName t => Lens' t Text
name = _Name . nameLocalName
{-# INLINE name #-}

-- | A Lens into node namespace
--
-- >>> ("<root/>" :: TL.Text) ^. xml.namespace
-- Nothing
--
-- >>> ("<root/>" :: TL.Text) & xml.namespace ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root xmlns=\"foo\"/>"
--
-- >>> ("<root xmlns=\"foo\"/>" :: TL.Text) & xml.namespace .~ Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
namespace :: AsName t => Lens' t (Maybe Text)
namespace = _Name . nameNamespace
{-# INLINE namespace #-}

-- | A Lens into node namespace
--
-- >>> ("<root/>" :: TL.Text) ^. xml.prefix
-- Nothing
--
-- >>> ("<root/>" :: TL.Text) & xml.prefix ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
--
-- >>> ("<root xmlns=\"foo\"/>" :: TL.Text) & xml.prefix ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo:root xmlns:foo=\"foo\"/>"
--
-- >>> ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo:root xmlns:foo=\"foo\"/>" :: TL.Text) & xml.prefix .~ Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root xmlns=\"foo\"/>"
prefix :: AsName t => Lens' t (Maybe Text)
prefix = _Name . namePrefix
{-# INLINE prefix #-}

-- | @xml-conduit@ entity resolving exceptions overloading
class AsUnresolvedEntityException p f t where
  _UnresolvedEntityException :: Overloaded' p f t UnresolvedEntityException

instance AsUnresolvedEntityException p f UnresolvedEntityException where
  _UnresolvedEntityException = id
  {-# INLINE _UnresolvedEntityException #-}

instance (Applicative f, Choice p) => AsUnresolvedEntityException p f SomeException where
  _UnresolvedEntityException = exception
  {-# INLINE _UnresolvedEntityException #-}

class AsXMLException p f t where
  _XMLException :: Overloaded' p f t XMLException

instance AsXMLException p f XMLException where
  _XMLException = id
  {-# INLINE _XMLException #-}

instance (Applicative f, Choice p) => AsXMLException p f SomeException where
  _XMLException = exception
  {-# INLINE _XMLException #-}

-- | @xml-conduit@ XML parsing exceptions overloading
class AsInvalidEventStream p f t where
  _InvalidEventStream :: Overloaded' p f t InvalidEventStream

instance AsInvalidEventStream p f InvalidEventStream where
  _InvalidEventStream = id

instance (Applicative f, Choice p) => AsInvalidEventStream p f SomeException where
  _InvalidEventStream = exception

-- | A Prism into 'ContentAfterRoot'
_ContentAfterRoot
  :: (Applicative f, Choice p, AsInvalidEventStream p f t)
  => Overloaded' p f t EventPos
_ContentAfterRoot = _InvalidEventStream
  . prism' ContentAfterRoot (\s -> case s of ContentAfterRoot e -> Just e; _ -> Nothing)
{-# INLINE _ContentAfterRoot #-}

-- | A Prism into 'MissingRootElement'
_MissingRootElement
  :: (Applicative f, Choice p, AsInvalidEventStream p f t)
  => Overloaded' p f t ()
_MissingRootElement = _InvalidEventStream
  . prism' (const MissingRootElement) (\s -> case s of MissingRootElement -> Just (); _ -> Nothing)
{-# INLINE _MissingRootElement #-}

-- | A Prism into 'InvalidInlineDoctype'
_InvalidInlineDoctype
  :: (Applicative f, Choice p, AsInvalidEventStream p f t)
  => Overloaded' p f t EventPos
_InvalidInlineDoctype = _InvalidEventStream
  . prism' InvalidInlineDoctype (\s -> case s of InvalidInlineDoctype e -> Just e; _ -> Nothing)
{-# INLINE _InvalidInlineDoctype #-}

-- | A Prism into 'MissingEndElement'
_MissingEndElement
  :: (Applicative f, Choice p, AsInvalidEventStream p f t)
  => Overloaded' p f t (Name, Maybe EventPos)
_MissingEndElement = _InvalidEventStream
  . prism' (uncurry MissingEndElement) (\s -> case s of MissingEndElement e p -> Just (e, p); _ -> Nothing)
{-# INLINE _MissingEndElement #-}

-- | A Prism into 'UnterminatedInlineDoctype'
_UnterminatedInlineDoctype
  :: (Applicative f, Choice p, AsInvalidEventStream p f t)
  => Overloaded' p f t ()
_UnterminatedInlineDoctype = _InvalidEventStream
  . prism' (const UnterminatedInlineDoctype) (\s -> case s of UnterminatedInlineDoctype -> Just (); _ -> Nothing)
{-# INLINE _UnterminatedInlineDoctype #-}
