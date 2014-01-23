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
  ( -- * Optics for 'Doctype'
    Doctype
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
    -- * Optics for 'Document'
  , Document
  , AsDocument(..)
  , root
  , prologue
  , epilogue
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
  ( Document, Doctype, Prologue
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

-- | Traverse itself with its all children.　Rewriting subnodes of each children will break a traversal law.
entire :: Traversal' Element Element
entire f e@(Element _ _ ns) = com <$> f e <*> traverse (_NodeElement (entire f)) ns where
    com (Element n a _) = Element n a

class AsDocument t where
  -- | A 'Prism'' into XML 'Document'
  _Document :: Prism' t Document

instance AsDocument Document where
  _Document = id
  {-# INLINE _Document #-}

instance AsDocument BL.ByteString where
  _Document = prism' (renderLBS def) (either (const Nothing) Just . parseLBS def)
  {-# INLINE _Document #-}

instance AsDocument TL.Text where
  _Document = prism' (renderText def) (either (const Nothing) Just . parseText def)
  {-# INLINE _Document #-}

prologue :: AsDocument t => Traversal' t Prologue
prologue = _Document . documentPrologue
{-# INLINE prologue #-}

root :: AsDocument t => Traversal' t Element
root = _Document . documentRoot
{-# INLINE root #-}

epilogue :: AsDocument t => Traversal' t [Miscellaneous]
epilogue = _Document . documentEpilogue
{-# INLINE epilogue #-}

doctype :: AsDocument t => Traversal' t (Maybe Doctype)
doctype = prologue . prologueDoctype
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

instance Plated Element where
  plate = elementNodes . traverse . _NodeElement
  {-# INLINE plate #-}

-- | Look into the nodes one level deeper with specific names
--
-- >>> let xml = "<root><foo>boo</foo><foo>hoo</foo><bar>moo</bar></root>" :: TL.Text
--
-- >>> xml ^. root.node "root".node "foo".text
-- "boohoo"
--
-- >>> xml ^? root.node "root".node "bar".text
-- Just "moo"
--
-- >>> xml ^? root.node "root".node "baz".text
-- Nothing
node :: AsElement t => Name -> Traversal' t Node
node n = named n . elementNodes . traverse
{-# INLINE node #-}

-- | Look into the nodes one level deeper
--
-- >>> let xml = "<root><foo>boo</foo><foo>hoo</foo><bar>moo</bar></root>" :: TL.Text
--
-- >>> xml ^. root.node "root".nodes.text
-- "boohoomoo"
--
-- >>> xml ^? root.node "moot".nodes.text
-- Nothing
nodes :: AsElement t => Traversal' t Node
nodes = _Element . elementNodes . traverse
{-# INLINE nodes #-}

-- | Select nodes by name
--
-- >>> let xml = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> xml ^.. root.node "root".named "foo".name
-- ["foo","foo"]
--
-- >>> xml ^? root.node "root".named "bar".name
-- Just "bar"
--
-- >>> xml ^? root.node "root".named "baz".name
-- Nothing
named :: AsElement t => Name -> Traversal' t Element
named n = _Element . filtered (has (elementName.only n))
{-# INLINE named #-}

-- | Select nodes by attributes' values
--
-- >>> let xml = "<root><foo bar=\"baz\">4</foo><foo bar=\"quux\">7</foo><bar bar=\"baz\">11</bar></root>" :: TL.Text
--
-- >>> xml ^.. root.plate.attributed (ix "bar".only "baz").nodes.text
-- ["4","11"]
--
-- >>> xml ^? root.plate.attributed (folded.to Text.length.only 4).nodes.text
-- Just "7"
attributed :: AsElement t => Fold (Map Name Text) a -> Traversal' t Element
attributed p = _Element . filtered (has (elementAttributes . p))
{-# INLINE attributed #-}

-- | Node text contents
--
-- >>> let xml = "<root>boo</root>" :: TL.Text
--
-- >>> xml ^? root.node "root".text
-- Just "boo"
--
-- >>> xml & root.node "root".text <>~ "hoo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>boohoo</root>"
text :: Prism' Node Text
text = _NodeContent
{-# INLINE text #-}

-- | Select node attributes by name
--
-- >>> let xml = "<root><foo bar=\"baz\" qux=\"quux\"/><foo qux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> xml ^.. root.node "root".attr "qux"
-- ["quux","xyzzy"]
--
-- >>> xml ^.. root.node "root".attr "bar"
-- ["baz"]
--
-- >>> xml & root.node "root".attr "qux" %~ Text.reverse
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"baz\" qux=\"xuuq\"/><foo qux=\"yzzyx\"/></root>"
attr :: AsElement t => Name -> IndexedTraversal' Name t Text
attr n = _Element . elementAttributes . ix n
{-# INLINE attr #-}

-- | Traverse node attributes
--
-- >>> let xml = "<root><foo bar=\"baz\" qux=\"zap\"/><foo quux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> xml ^.. root.node "root".attrs.indices (has (name.unpacked.prefixed "qu"))
-- ["zap","xyzzy"]
--
-- >>> xml & root.node "root".attrs %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"BAZ\" qux=\"ZAP\"/><foo quux=\"XYZZY\"/></root>"
attrs :: AsElement t => IndexedTraversal' Name t Text
attrs = _Element . elementAttributes . itraversed
{-# INLINE attrs #-}

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
instance Plated Node where
  plate = nodes
  {-# INLINE plate #-}

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
  _Name :: Traversal' t Name

instance AsName Name where
  _Name = id
  {-# INLINE _Name #-}

instance AsName Element where
  _Name = elementName
  {-# INLINE _Name #-}

instance AsName Node where
  _Name = _NodeElement . _Name
  {-# INLINE _Name #-}

name :: AsName t => Traversal' t Text
name = _Name . nameLocalName
{-# INLINE name #-}

namespace :: AsName t => Traversal' t (Maybe Text)
namespace = _Name . nameNamespace
{-# INLINE namespace #-}

prefix :: AsName t => Traversal' t (Maybe Text)
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
