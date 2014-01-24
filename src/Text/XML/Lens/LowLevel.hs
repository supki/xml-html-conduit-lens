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
-- Lenses and Prisms Template Haskell could handle
----------------------------------------------------------------------------
module Text.XML.Lens.LowLevel where

import           Control.Lens
import           Data.Map (Map)
import           Data.Text (Text)
import           Text.XML
  ( ParseSettings, RenderSettings
  , Document, Doctype, Prologue, ExternalID
  , Node(..), Element, Instruction, Name, Miscellaneous(..)
  )
import           Text.XML.Stream.Parse (DecodeEntities)
import qualified Text.XML as XML

-- | A Lens into 'XML.rsPretty'
psDecodeEntities :: Lens' ParseSettings DecodeEntities
psDecodeEntities f ps = f (XML.psDecodeEntities ps) <&> \p -> ps { XML.psDecodeEntities = p }
{-# INLINE psDecodeEntities #-}

-- | A Lens into 'XML.rsPretty'
rsPretty :: Lens' RenderSettings Bool
rsPretty f rs = f (XML.rsPretty rs) <&> \p -> rs { XML.rsPretty = p }
{-# INLINE rsPretty #-}

-- | A Lens into 'XML.rsNamespaces'
rsNamespaces :: Lens' RenderSettings [(Text, Text)]
rsNamespaces f rs = f (XML.rsNamespaces rs) <&> \p -> rs { XML.rsNamespaces = p }
{-# INLINE rsNamespaces #-}

-- | A Lens into 'XML.rsAttrOrder'
rsAttrOrder :: Lens' RenderSettings (Name -> Map Name Text -> [(Name, Text)])
rsAttrOrder f rs = f (XML.rsAttrOrder rs) <&> \p -> rs { XML.rsAttrOrder = p }
{-# INLINE rsAttrOrder #-}

-- | A Lens into 'XML.documentPrologue'
documentPrologue :: Lens' Document Prologue
documentPrologue f doc = f (XML.documentPrologue doc) <&> \p -> doc { XML.documentPrologue = p }
{-# INLINE documentPrologue #-}

-- | A Lens into 'XML.documentRoot'
documentRoot :: Lens' Document Element
documentRoot f doc = f (XML.documentRoot doc) <&> \p -> doc { XML.documentRoot = p }
{-# INLINE documentRoot #-}

-- | A Lens into 'XML.documentEpilogue'
documentEpilogue :: Lens' Document [Miscellaneous]
documentEpilogue f doc =  f (XML.documentEpilogue doc) <&> \p -> doc { XML.documentEpilogue = p }
{-# INLINE documentEpilogue #-}

-- | A Lens into 'XML.prologueBefore'
prologueBefore :: Lens' Prologue [Miscellaneous]
prologueBefore f doc =  f (XML.prologueBefore doc) <&> \p -> doc { XML.prologueBefore = p }
{-# INLINE prologueBefore #-}

-- | A Lens into 'XML.prologueDoctype'
prologueDoctype :: Lens' Prologue (Maybe Doctype)
prologueDoctype f doc =  f (XML.prologueDoctype doc) <&> \p -> doc { XML.prologueDoctype = p }
{-# INLINE prologueDoctype #-}

-- | A Lens into 'XML.prologueAfter'
prologueAfter :: Lens' Prologue [Miscellaneous]
prologueAfter f doc =  f (XML.prologueAfter doc) <&> \p -> doc { XML.prologueAfter = p }
{-# INLINE prologueAfter #-}

-- | A Lens into 'XML.doctypeName'
doctypeName :: Lens' Doctype Text
doctypeName f doc =  f (XML.doctypeName doc) <&> \p -> doc { XML.doctypeName = p }
{-# INLINE doctypeName #-}

-- | A Lens into 'XML.doctypeID'
doctypeID :: Lens' Doctype (Maybe ExternalID)
doctypeID f doc =  f (XML.doctypeID doc) <&> \p -> doc { XML.doctypeID = p }
{-# INLINE doctypeID #-}

-- | A Prism into 'XML.SystemID'
_SystemID :: Prism' ExternalID Text
_SystemID = prism' XML.SystemID (\s -> case s of XML.SystemID e -> Just e; _ -> Nothing)
{-# INLINE _SystemID #-}

-- | A Prism into 'XML.SystemID'
_PublicID :: Prism' ExternalID (Text, Text)
_PublicID = prism' (uncurry XML.PublicID) (\s -> case s of XML.PublicID e e' -> Just (e, e'); _ -> Nothing)
{-# INLINE _PublicID #-}

-- | A Lens into 'XML.elementName'
elementName :: Lens' Element Name
elementName f e = f (XML.elementName e) <&> \p -> e { XML.elementName = p }
{-# INLINE elementName #-}

-- | A Lens into 'XML.elementAttributes'
elementAttributes :: Lens' Element (Map Name Text)
elementAttributes f e = f (XML.elementAttributes e) <&> \p -> e { XML.elementAttributes = p }
{-# INLINE elementAttributes #-}

-- | A Lens into 'XML.elementNodes'
elementNodes :: Lens' Element [Node]
elementNodes f e = f (XML.elementNodes e) <&> \p -> e { XML.elementNodes = p }
{-# INLINE elementNodes #-}

-- | A Lens into 'XML.nameLocalName'
nameLocalName :: Lens' Name Text
nameLocalName f n = f (XML.nameLocalName n) <&> \p -> n { XML.nameLocalName = p }
{-# INLINE nameLocalName #-}

-- | A Lens into 'XML.nameNamespace'
nameNamespace :: Lens' Name (Maybe Text)
nameNamespace f n = f (XML.nameNamespace n) <&> \p -> n { XML.nameNamespace = p }
{-# INLINE nameNamespace #-}

-- | A Lens into 'XML.namePrefix'
namePrefix :: Lens' Name (Maybe Text)
namePrefix f n = f (XML.namePrefix n) <&> \p -> n { XML.namePrefix = p }
{-# INLINE namePrefix #-}

-- | A Lens into 'XML.instructionTarget'
instructionTarget :: Lens' Instruction Text
instructionTarget f i = f (XML.instructionTarget i) <&> \p -> i { XML.instructionTarget = p }
{-# INLINE instructionTarget #-}

-- | A Lens into 'XML.instructionData'
instructionData :: Lens' Instruction Text
instructionData f i = f (XML.instructionData i) <&> \p -> i { XML.instructionData = p }
{-# INLINE instructionData #-}

-- | A Prism into 'XML.NodeElement'
_NodeElement :: Prism' Node Element
_NodeElement = prism' NodeElement (\s -> case s of NodeElement e -> Just e; _ -> Nothing)
{-# INLINE _NodeElement #-}

-- | A Prism into 'XML.NodeContent'
_NodeContent :: Prism' Node Text
_NodeContent = prism' NodeContent (\s -> case s of NodeContent e -> Just e; _ -> Nothing)
{-# INLINE _NodeContent #-}

-- | A Prism into 'XML.NodeInstruction'
_NodeInstruction :: Prism' Node Instruction
_NodeInstruction = prism' NodeInstruction (\s -> case s of NodeInstruction e -> Just e; _ -> Nothing)
{-# INLINE _NodeInstruction #-}

-- | A Prism into 'XML.NodeComment'
_NodeComment :: Prism' Node Text
_NodeComment = prism' NodeComment (\s -> case s of NodeComment e -> Just e; _ -> Nothing)
{-# INLINE _NodeComment #-}

-- | A Prism into 'XML.MiscComment'
_MiscComment :: Prism' Miscellaneous Text
_MiscComment = prism' MiscComment (\s -> case s of MiscComment e -> Just e; _ -> Nothing)
{-# INLINE _MiscComment #-}

-- | A Prism into 'XML.MiscInstruction'
_MiscInstruction :: Prism' Miscellaneous Instruction
_MiscInstruction = prism' MiscInstruction (\s -> case s of MiscInstruction e -> Just e; _ -> Nothing)
{-# INLINE _MiscInstruction #-}
