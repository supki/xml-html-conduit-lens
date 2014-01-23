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
  ( -- * Optics for 'Document'
    Document
  , AsDocument(..)
  , root
  , prologue
  , epilogue
    -- * Optics for 'Doctype'
  , Doctype
  , doctype
    -- * Optics for 'Element'
  , Element
  , AsElement(..)
  , nodes
  , node
  , named
  , attributed
  , attr
  , attrs
    -- * Prisms for 'Node'
  , Node
  , text
  , AsComment(..)
  , instruction
    -- ** Children
  , entire
    -- * Optics for 'Name'
  , Name
  , AsName(..)
  , name
  , namespace
  , prefix
    -- * Optics for 'Instruction'
  , Instruction
  , AsProcessingInstruction(..)
  , target
  , data_
    -- * Optics for exceptions
  , UnresolvedEntityException
  , XMLException
  , AsUnresolvedEntityException(..)
  , AsXMLException(..)
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
  , Element(Element), Instruction, Name, Miscellaneous(..)
  , XMLException(..), UnresolvedEntityException(..)
  , parseLBS, parseText, renderLBS, renderText, def
  )

import Text.XML.Lens.LowLevel

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import           Data.List.Lens (prefixed)
-- >>> import           Data.Text.Lens (unpacked)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Text.XML as XML

-- | Traverse itself with its all children.　Rewriting subnodes of each children will break a traversal law.
entire :: Traversal' Element Element
entire f e@(Element _ _ ns) = com <$> f e <*> traverse (_NodeElement (entire f)) ns where
    com (Element n a _) = Element n a

-- | A 'Prism'' into XML 'Document'
class AsDocument t where
  _DocumentWith :: ParseSettings -> RenderSettings -> Prism' t Document

instance AsDocument Document where
  _DocumentWith _ _ = id
  {-# INLINE _DocumentWith #-}

instance AsDocument BL.ByteString where
  _DocumentWith ps rs = prism' (renderLBS rs) (either (const Nothing) Just . parseLBS ps)
  {-# INLINE _DocumentWith #-}

instance AsDocument TL.Text where
  _DocumentWith ps rs = prism' (renderText rs) (either (const Nothing) Just . parseText ps)
  {-# INLINE _DocumentWith #-}

_Document :: AsDocument t => Prism' t Document
_Document = _DocumentWith def def
{-# INLINE _Document #-}

prologue :: AsDocument t => Traversal' t Prologue
prologue = _Document . documentPrologue
{-# INLINE prologue #-}

-- | A Traversal into XML document root node
--
-- >>> ("<foo/>" :: TL.Text) ^? root.name
-- Just "foo"
--
-- >>> ("<foo><bar/><baz/></foo>" :: TL.Text) ^? root.name
-- Just "foo"
--
-- >>> ("<foo/>" :: TL.Text) & root.name .~ "boo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><boo/>"
root :: AsDocument t => Traversal' t Element
root = _Document . documentRoot
{-# INLINE root #-}

epilogue :: AsDocument t => Traversal' t [Miscellaneous]
epilogue = _Document . documentEpilogue
{-# INLINE epilogue #-}

-- | A Lens into XML DOCTYPE declaration
--
-- >>> let xml = "<!DOCTYPE foo><root/>" :: TL.Text
--
-- >>> xml ^? prologue.doctype.folded.doctypeName
-- Just "foo"
--
-- >>> xml & prologue.doctype.traverse.doctypeName .~ "moo"
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

class AsElement t where
  _Element :: Prism' t Element

instance AsElement Element where
  _Element = id
  {-# INLINE _Element #-}

instance AsElement Node where
  _Element = _NodeElement
  {-# INLINE _Element #-}

type instance Index Element = Name
type instance IxValue Element = Text

instance At Element where
  at n = elementAttributes . at n
  {-# INLINE at #-}

instance Applicative f => Ixed f Element where
  ix n = elementAttributes . ix n
  {-# INLINE ix #-}

-- | Look into the nodes one level deeper
--
-- @
-- plate ≡ nodes
-- @
--
-- >>> let xml = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> xml ^.. root.plate.name
-- ["foo","foo","bar"]
--
-- >>> xml & partsOf (root.plate.name) .~ ["boo", "hoo", "moo"]
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><boo>4</boo><hoo>7</hoo><moo>11</moo></root>"
instance Plated Element where
  plate = elementNodes . traverse . _NodeElement
  {-# INLINE plate #-}

-- | Look into the nodes one level deeper with specific names
--
-- >>> let xml = "<root><foo>boo</foo><foo>hoo</foo><bar>moo</bar></root>" :: TL.Text
--
-- >>> xml ^. root.node "foo".text
-- "boohoo"
--
-- >>> xml ^? root.node "bar".text
-- Just "moo"
--
-- >>> xml ^? root.node "baz".text
-- Nothing
node :: AsElement t => Name -> Traversal' t Element
node n = _Element . elementNodes . traverse . named n
{-# INLINE node #-}

-- | Look into the nodes one level deeper
--
-- >>> let xml = "<root><foo>boo</foo><foo>hoo</foo><bar>moo</bar></root>" :: TL.Text
--
-- >>> xml ^. root.nodes.text
-- "boohoomoo"
--
-- >>> xml ^? root.node "moot".text
-- Nothing
nodes :: AsElement t => Traversal' t Element
nodes = _Element . plate
{-# INLINE nodes #-}

-- | Select nodes by name
--
-- >>> let xml = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> xml ^.. root.nodes.named "foo".name
-- ["foo","foo"]
--
-- >>> xml ^? root.nodes.named "bar".name
-- Just "bar"
--
-- >>> xml ^? root.nodes.named "baz".name
-- Nothing
named :: AsElement t => Name -> Traversal' t Element
named n = _Element . filtered (has (elementName.only n))
{-# INLINE named #-}

-- | Select nodes by attributes' values
--
-- >>> let xml = "<root><foo bar=\"baz\">4</foo><foo bar=\"quux\">7</foo><bar bar=\"baz\">11</bar></root>" :: TL.Text
--
-- >>> xml ^.. root.plate.attributed (ix "bar".only "baz").text
-- ["4","11"]
--
-- >>> xml ^? root.plate.attributed (folded.to Text.length.only 4).text
-- Just "7"
attributed :: AsElement t => Fold (Map Name Text) a -> Traversal' t Element
attributed p = _Element . filtered (has (elementAttributes . p))
{-# INLINE attributed #-}

-- | Node text contents
--
-- >>> let xml = "<root>boo</root>" :: TL.Text
--
-- >>> xml ^? root.text
-- Just "boo"
--
-- >>> xml & root.text <>~ "hoo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>boohoo</root>"
text :: Traversal' Element Text
text = elementNodes . traverse . _NodeContent
{-# INLINE text #-}

-- | Select node attributes by name
--
-- >>> let xml = "<root><foo bar=\"baz\" qux=\"quux\"/><foo qux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> xml ^.. root.plate.attr "qux"
-- ["quux","xyzzy"]
--
-- >>> xml ^.. root.plate.attr "bar"
-- ["baz"]
--
-- >>> xml & root.plate.attr "qux" %~ Text.reverse
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"baz\" qux=\"xuuq\"/><foo qux=\"yzzyx\"/></root>"
attr :: AsElement t => Name -> IndexedTraversal' Name t Text
attr n = _Element . elementAttributes . ix n
{-# INLINE attr #-}

-- | Traverse node attributes
--
-- >>> let xml = "<root><foo bar=\"baz\" qux=\"zap\"/><foo quux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> xml ^.. root.plate.attrs.indices (has (name.unpacked.prefixed "qu"))
-- ["zap","xyzzy"]
--
-- >>> xml & root.plate.attrs %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"BAZ\" qux=\"ZAP\"/><foo quux=\"XYZZY\"/></root>"
attrs :: AsElement t => IndexedTraversal' Name t Text
attrs = _Element . elementAttributes . itraversed
{-# INLINE attrs #-}

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
  _Instruction = prism' MiscInstruction (\s -> case s of MiscInstruction e -> Just e; _ -> Nothing)
  {-# INLINE _Instruction #-}

target :: AsProcessingInstruction t => Traversal' t Text
target = _Instruction . instructionTarget
{-# INLINE target #-}

data_ :: AsProcessingInstruction t => Traversal' t Text
data_ = _Instruction . instructionData
{-# INLINE data_ #-}

-- | A 'Prism'' into XML comment
class AsComment t where
  comment :: Prism' t Text

instance AsComment Text where
  comment = id
  {-# INLINE comment #-}

instance AsComment Node where
  comment = _NodeComment
  {-# INLINE comment #-}

instance AsComment Miscellaneous where
  comment = prism' MiscComment (\s -> case s of MiscComment e -> Just e; _ -> Nothing)
  {-# INLINE comment #-}

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
-- >>> ("<root/>" :: TL.Text) ^. root.name
-- "root"
--
-- >>> ("<root><foo/><bar/><baz/></root>" :: TL.Text) ^.. root.plate.name
-- ["foo","bar","baz"]
--
-- >>> ("<root><foo/><bar/><baz></root>" :: TL.Text) & root.partsOf (plate.name) .~ ["boo", "hoo", "moo"]
-- "<root><foo/><bar/><baz></root>"
name :: AsName t => Lens' t Text
name = _Name . nameLocalName
{-# INLINE name #-}

-- | A Lens into node namespace
--
-- >>> ("<root/>" :: TL.Text) ^. root.namespace
-- Nothing
--
-- >>> ("<root/>" :: TL.Text) & root.namespace ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root xmlns=\"foo\"/>"
--
-- >>> ("<root xmlns=\"foo\"/>" :: TL.Text) & root.namespace .~ Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
namespace :: AsName t => Lens' t (Maybe Text)
namespace = _Name . nameNamespace
{-# INLINE namespace #-}

-- | A Lens into node namespace
--
-- >>> ("<root/>" :: TL.Text) ^. root.prefix
-- Nothing
--
-- >>> ("<root/>" :: TL.Text) & root.prefix ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
--
-- >>> ("<root xmlns=\"foo\"/>" :: TL.Text) & root.prefix ?~ "foo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo:root xmlns:foo=\"foo\"/>"
--
-- >>> ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo:root xmlns:foo=\"foo\"/>" :: TL.Text) & root.prefix .~ Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root xmlns=\"foo\"/>"
prefix :: AsName t => Lens' t (Maybe Text)
prefix = _Name . namePrefix
{-# INLINE prefix #-}

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
