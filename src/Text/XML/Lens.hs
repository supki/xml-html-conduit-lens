{-# LANGUAGE Rank2Types, FlexibleContexts #-}
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
    , name
    , localName
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
    , AsInstruction(..)
    , AsComment(..)
    -- * Lenses for 'Document'
    , Document
    , root
    , prologue
    , epilogue
    , doctype
    , documentPrologue
    , documentRoot
    , documentEpilogue
    -- * Lenses for 'Name'
    , Name(..)
    , _nameLocalName
    , _nameNamespace
    , _namePrefix
    -- * Lenses for 'Instruction'
    , Instruction(..)
    , _instructionTarget
    , _instructionData
    -- * Decoding
    , AsDocument(..)
    ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Map (Map)
import           Text.XML hiding (documentPrologue, documentRoot, documentEpilogue)
import qualified Text.XML as XML

infixr 9 ./

doctype :: Lens' Prologue (Maybe Doctype)
doctype f doc = fmap (\p -> doc { prologueDoctype = p} ) $ f $ prologueDoctype doc

class AsInstruction t where
    _Instruction :: Prism' t Instruction

_instructionTarget :: Lens' Instruction Text
_instructionTarget f (Instruction t d) = f t <&> \t' -> Instruction t' d

_instructionData :: Lens' Instruction Text
_instructionData f (Instruction t d) = f d <&> \d' -> Instruction t d'

instance AsInstruction Node where
    _Instruction = prism' NodeInstruction $ \s -> case s of
        NodeInstruction e -> Just e
        _ -> Nothing

instance AsInstruction Miscellaneous where
    _Instruction = prism' MiscInstruction $ \s -> case s of
        MiscInstruction e -> Just e
        _ -> Nothing

class AsComment t where
    _Comment :: Prism' t Text

instance AsComment Node where
    _Comment = prism' NodeComment $ \s -> case s of
        NodeComment e -> Just e
        _ -> Nothing

instance AsComment Miscellaneous where
    _Comment = prism' MiscComment $ \s -> case s of
        MiscComment e -> Just e
        _ -> Nothing

_nameLocalName :: Lens' Name Text
_nameLocalName f n = f (nameLocalName n) <&> \x -> n { nameLocalName = x }

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = f (nameNamespace n) <&> \x -> n { nameNamespace = x }

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = f (namePrefix n) <&> \x -> n { namePrefix = x }

_Element :: Prism' Node Element
_Element = prism' NodeElement $ \s -> case s of
    NodeElement e -> Just e
    _ -> Nothing

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \s -> case s of
    NodeContent e -> Just e
    _ -> Nothing

name :: Lens' Element Name
name f e = f (elementName e) <&> \x -> e { elementName = x }

localName :: Lens' Element Text
localName = name . _nameLocalName
{-# INLINE localName #-}

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
    | elementName s == n = f s
    | otherwise = pure s

-- | Traverse elements which has the specified *local* name. 
ell :: Text -> Traversal' Element Element
ell n f s
    | nameLocalName (elementName s) == n = f s
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
