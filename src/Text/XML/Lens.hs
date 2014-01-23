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
-- Useful traversals inspired by XPath
----------------------------------------------------------------------------
module Text.XML.Lens (
    -- * Optics for 'Doctype'
      Doctype
    , doctype
    , doctypeName
    , doctypeID
    -- * Optics for 'Element'
    , Element
    , AsElement(..)
    , nodes
    , node
    , named
    , attributes
    , elementName
    , elementAttributes
    , elementNodes
    -- * Prisms for 'Node'
    , Node
    , text
    , comment
    , instruction
    , _NodeElement
    , _NodeContent
    , _NodeComment
    , _NodeInstruction
    -- ** Stuff
    , attributed
    , attr
    , AsComment(..)
    -- ** Children
    , entire
    -- * Optics for 'Document'
    , Document
    , AsDocument(..)
    , root
    , prologue
    , epilogue
    , documentPrologue
    , documentRoot
    , documentEpilogue
    -- * Optics for 'Prologue'
    , prologueBefore
    , prologueDoctype
    , prologueAfter
    -- * Lenses for 'Name'
    , Name
    , AsName(..)
    , localName
    , namespace
    , prefix
    , nameLocalName
    , nameNamespace
    , namePrefix
    -- * Optics for 'Instruction'
    , Instruction
    , AsProcessingInstruction(..)
    , target
    , data_
    , instructionTarget
    , instructionData
    -- * Optics for exceptions
    , UnresolvedEntityException
    , XMLException
    , AsUnresolvedEntityException(..)
    , AsXMLException(..)
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
  ( Document, Doctype, Prologue, ExternalID
  , Node(..)
  , Element(Element), Instruction, Name, Miscellaneous(..)
  , XMLException(..), UnresolvedEntityException(..)
  , parseLBS, parseText, renderLBS, renderText, def
  )
import qualified Text.XML as XML

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

documentPrologue :: Lens' Document Prologue
documentPrologue f doc = f (XML.documentPrologue doc) <&> \p -> doc { XML.documentPrologue = p}
{-# INLINE documentPrologue #-}

documentRoot :: Lens' Document Element
documentRoot f doc = f (XML.documentRoot doc) <&> \p -> doc { XML.documentRoot = p}
{-# INLINE documentRoot #-}

documentEpilogue :: Lens' Document [Miscellaneous]
documentEpilogue f doc =  f (XML.documentEpilogue doc) <&> \p -> doc { XML.documentEpilogue = p}
{-# INLINE documentEpilogue #-}

doctype :: AsDocument t => Traversal' t (Maybe Doctype)
doctype = _Document . documentPrologue . prologueDoctype
{-# INLINE doctype #-}

prologueBefore :: Lens' Prologue [Miscellaneous]
prologueBefore f doc =  f (XML.prologueBefore doc) <&> \p -> doc { XML.prologueBefore = p }
{-# INLINE prologueBefore #-}

prologueDoctype :: Lens' Prologue (Maybe Doctype)
prologueDoctype f doc =  f (XML.prologueDoctype doc) <&> \p -> doc { XML.prologueDoctype = p }
{-# INLINE prologueDoctype #-}

prologueAfter :: Lens' Prologue [Miscellaneous]
prologueAfter f doc =  f (XML.prologueAfter doc) <&> \p -> doc { XML.prologueAfter = p }
{-# INLINE prologueAfter #-}

doctypeName :: Lens' Doctype Text
doctypeName f doc =  f (XML.doctypeName doc) <&> \p -> doc { XML.doctypeName = p }
{-# INLINE doctypeName #-}

doctypeID :: Lens' Doctype (Maybe ExternalID)
doctypeID f doc =  f (XML.doctypeID doc) <&> \p -> doc { XML.doctypeID = p }
{-# INLINE doctypeID #-}

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

nodes :: AsElement t => Traversal' t Node
nodes = _Element . elementNodes . traverse
{-# INLINE nodes #-}

node :: AsElement t => Name -> Traversal' t Node
node n = named n . elementNodes . traverse
{-# INLINE node #-}

-- | Traverse elements which has the specified name.
named :: AsElement t => Name -> Traversal' t Element
named n = _Element . filtered (has (elementName.only n))
{-# INLINE named #-}

attributed :: AsElement t => Fold (Map Name Text) a -> Traversal' t Element
attributed p = _Element . filtered (has (elementAttributes . p))
{-# INLINE attributed #-}

attributes :: AsElement t => Traversal' t (Map Name Text)
attributes = _Element . elementAttributes
{-# INLINE attributes #-}

_NodeElement :: Prism' Node Element
_NodeElement = prism' NodeElement (\s -> case s of NodeElement e -> Just e; _ -> Nothing)
{-# INLINE _NodeElement #-}

_NodeComment :: Prism' Node Text
_NodeComment = prism' NodeComment (\s -> case s of NodeComment e -> Just e; _ -> Nothing)
{-# INLINE _NodeComment #-}

_NodeContent :: Prism' Node Text
_NodeContent = prism' NodeContent (\s -> case s of NodeContent e -> Just e; _ -> Nothing)
{-# INLINE _NodeContent #-}

_NodeInstruction :: Prism' Node Instruction
_NodeInstruction = prism' NodeInstruction (\s -> case s of NodeInstruction e -> Just e; _ -> Nothing)
{-# INLINE _NodeInstruction #-}

elementName :: Lens' Element Name
elementName f e = f (XML.elementName e) <&> \p -> e { XML.elementName = p }
{-# INLINE elementName #-}

elementAttributes :: Lens' Element (Map Name Text)
elementAttributes f e = f (XML.elementAttributes e) <&> \p -> e { XML.elementAttributes = p }
{-# INLINE elementAttributes #-}

elementNodes :: Lens' Element [Node]
elementNodes f e = f (XML.elementNodes e) <&> \p -> e { XML.elementNodes = p }
{-# INLINE elementNodes #-}

attr :: AsElement t => Name -> IndexedTraversal' Name t Text
attr n = attributes . ix n
{-# INLINE attr #-}

-- | Traverse node text.
text :: Traversal' Node Text
text = _NodeContent
{-# INLINE text #-}

-- | Traverse node comment.
comment :: Traversal' Node Text
comment = _NodeComment
{-# INLINE comment #-}

-- | Traverse node instruction.
instruction :: Traversal' Node Instruction
instruction = _NodeInstruction
{-# INLINE instruction #-}

-- | A 'Prism'' into processing 'Instruction'
class AsProcessingInstruction t where
  _Instruction :: Prism' t Instruction

instance AsProcessingInstruction Instruction where
  _Instruction = id
  {-# INLINE _Instruction #-}

instance AsProcessingInstruction Node where
  _Instruction = _NodeInstruction
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

instructionTarget :: Lens' Instruction Text
instructionTarget f i = f (XML.instructionTarget i) <&> \p -> i { XML.instructionTarget = p }
{-# INLINE instructionTarget #-}

instructionData :: Lens' Instruction Text
instructionData f i = f (XML.instructionData i) <&> \p -> i { XML.instructionData = p }
{-# INLINE instructionData #-}

-- | A 'Prism'' into XML comment
class AsComment t where
    _Comment :: Prism' t Text

instance AsComment Text where
  _Comment = id
  {-# INLINE _Comment #-}

instance AsComment Node where
  _Comment = _NodeComment
  {-# INLINE _Comment #-}

instance AsComment Miscellaneous where
  _Comment = prism' MiscComment (\s -> case s of MiscComment e -> Just e; _ -> Nothing)
  {-# INLINE _Comment #-}

class AsName t where
  name :: Traversal' t Name

instance AsName Name where
  name = id
  {-# INLINE name #-}

instance AsName Element where
  name = elementName
  {-# INLINE name #-}

localName :: AsName t => Traversal' t Text
localName = name . nameLocalName
{-# INLINE localName #-}

namespace :: AsName t => Traversal' t (Maybe Text)
namespace = name . nameNamespace
{-# INLINE namespace #-}

prefix :: AsName t => Traversal' t (Maybe Text)
prefix = name . namePrefix
{-# INLINE prefix #-}

nameLocalName :: Lens' Name Text
nameLocalName f n = f (XML.nameLocalName n) <&> \p -> n { XML.nameLocalName = p }
{-# INLINE nameLocalName #-}

nameNamespace :: Lens' Name (Maybe Text)
nameNamespace f n = f (XML.nameNamespace n) <&> \p -> n { XML.nameNamespace = p }
{-# INLINE nameNamespace #-}

namePrefix :: Lens' Name (Maybe Text)
namePrefix f n = f (XML.namePrefix n) <&> \p -> n { XML.namePrefix = p }
{-# INLINE namePrefix #-}

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
