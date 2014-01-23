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
    -- * Lenses, traversals for 'Element'
    Element(..)
    , (./)
    -- ** Names
    , el
    , ell
    -- ** Attributes
    , attributeIs
    , attributeSatisfies
    , attr
    , attribute
    , attrs
    -- ** Contents
    , text
    , comment
    -- ** Children
    , entire
    , nodes
    -- * Prisms for 'Node'
    , Node(..)
    , _Element
    , _Content
    , AsComment(..)
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
    -- * Optics for 'Doctype'
    , Doctype
    , doctype
    , doctypeName
    , doctypeID
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
import           Text.XML hiding
  ( documentPrologue, documentRoot, documentEpilogue
  , prologueBefore, prologueDoctype, prologueAfter
  , instructionTarget, instructionData
  , doctypeName, doctypeID
  , elementName, nameLocalName, nameNamespace, namePrefix
  )
import qualified Text.XML as XML

infixr 9 ./

_Element :: Prism' Node Element
_Element = prism' NodeElement $ \s -> case s of
    NodeElement e -> Just e
    _ -> Nothing

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \s -> case s of
    NodeContent e -> Just e
    _ -> Nothing

elementName :: Lens' Element Name
elementName f e = f (XML.elementName e) <&> \x -> e { XML.elementName = x }

attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e

nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e

attr :: Name -> IndexedTraversal' Name Element Text
attr n = attrs . ix n

attribute :: Name -> IndexedLens' Name Element (Maybe Text)
attribute n = attrs . at n

-- | Traverse itself with its all children.　Rewriting subnodes of each children will break a traversal law.
entire :: Traversal' Element Element
entire f e@(Element _ _ ns) = com <$> f e <*> traverse (_Element (entire f)) ns where
    com (Element n a _) = Element n a

-- | Traverse elements which has the specified name.
el :: Name -> Traversal' Element Element
el n f s
    | XML.elementName s == n = f s
    | otherwise = pure s

-- | Traverse elements which has the specified *local* name. 
ell :: Text -> Traversal' Element Element
ell n f s
    | XML.nameLocalName (XML.elementName s) == n = f s
    | otherwise = pure s

attributeSatisfies :: Name -> (Text -> Bool) -> Traversal' Element Element
attributeSatisfies n p = filtered (maybe False p . preview (attrs . ix n))

attributeIs :: Name -> Text -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)

-- | Traverse all contents of the element.
text :: Traversal' Element Text
text = nodes . traverse . _Content

-- | Traverse all comments of the element.
comment :: Traversal' Element Text
comment = nodes . traverse . _Comment

instance Plated Element where
    plate = nodes . traverse . _Element

-- | Combine two 'Traversal's just like XPath's slash.
-- 
-- @ 
-- l ./ m ≡ l . 'plate' . m
-- @
(./) :: Plated a => Traversal s t a a -> Traversal a a u v -> Traversal s t u v
l ./ m = l . plate . m
{-# INLINE (./) #-}

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

-- | A 'Prism'' into processing 'Instruction'
class AsProcessingInstruction t where
  _Instruction :: Prism' t Instruction

instance AsProcessingInstruction Instruction where
  _Instruction = id
  {-# INLINE _Instruction #-}

instance AsProcessingInstruction Node where
  _Instruction = prism' NodeInstruction (\s -> case s of NodeInstruction e -> Just e; _ -> Nothing)
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
  _Comment = prism' NodeComment (\s -> case s of NodeComment e -> Just e; _ -> Nothing)
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
nameLocalName f n = f (XML.nameLocalName n) <&> \x -> n { XML.nameLocalName = x }
{-# INLINE nameLocalName #-}

nameNamespace :: Lens' Name (Maybe Text)
nameNamespace f n = f (XML.nameNamespace n) <&> \x -> n { XML.nameNamespace = x }
{-# INLINE nameNamespace #-}

namePrefix :: Lens' Name (Maybe Text)
namePrefix f n = f (XML.namePrefix n) <&> \x -> n { XML.namePrefix = x }
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
