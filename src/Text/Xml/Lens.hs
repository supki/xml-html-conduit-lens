{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Optics for xml-conduit and html-conduit
module Text.Xml.Lens
  ( -- * Document
    Document
  , xml
  , html
  , root
  , renderWith
  , render
  , Prologue
  , prolog
  , epilog
  , AsXmlDocument(..)
  , _XmlDocument
  , AsHtmlDocument(..)
    -- * Doctype
  , Doctype
  , doctype
  , beforeDoctype
  , afterDoctype
    -- * Element
  , Element
  , node
  , named
  , attrs
  , attr
  , attributed
  , text
  , HasComments(..)
  , HasInstructions(..)
    -- * Name
  , Name
  , name
  , namespace
  , prefix
  , HasName(..)
    -- * Instruction
  , Instruction
  , target
  , data_
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
  , module Text.Xml.Lens.LowLevel
  ) where

import           Control.Exception (SomeException)
import           Control.Exception.Lens (exception)
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Map (Map)
import           Text.XML
  ( ParseSettings, RenderSettings
  , Document(Document), Doctype, Prologue(Prologue)
  , Element, Node, Instruction, Name, Miscellaneous(..)
  , XMLException(..), UnresolvedEntityException(..)
  , parseLBS, parseText, renderLBS, renderText, def
  )
import           Text.XML.Stream.Parse (EventPos)
import           Text.XML.Unresolved (InvalidEventStream(..))
import qualified Text.HTML.DOM as Html

import Text.Xml.Lens.LowLevel

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import           Data.List.Lens (prefixed)
-- >>> import           Data.Text.Lens (unpacked)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Text.XML as XML

-- | XML document parsing and rendering overloading
--
-- This is a general version; for parsing/rendering with the
-- default options see '_XmlDocument'
class AsXmlDocument t where
  _XmlDocumentWith
    :: (ParseSettings -> ParseSettings) -> (RenderSettings -> RenderSettings) -> Prism' t Document

instance AsXmlDocument Document where
  _XmlDocumentWith _ _ = id
  {-# INLINE _XmlDocumentWith #-}

instance AsXmlDocument BL.ByteString where
  _XmlDocumentWith p r = prism' (renderLBS (r def)) (either (const Nothing) Just . parseLBS (p def))
  {-# INLINE _XmlDocumentWith #-}

instance AsXmlDocument TL.Text where
  _XmlDocumentWith p r = prism' (renderText (r def)) (either (const Nothing) Just . parseText (p def))
  {-# INLINE _XmlDocumentWith #-}

-- | XML document parsing and rendering with the default settings
_XmlDocument :: AsXmlDocument t => Prism' t Document
_XmlDocument = _XmlDocumentWith def def
{-# INLINE _XmlDocument #-}

-- | HTML document parsing overloading
class AsHtmlDocument t where
  _HtmlDocument :: Fold t Document

instance AsHtmlDocument Document where
  _HtmlDocument = id
  {-# INLINE _HtmlDocument #-}

instance AsHtmlDocument BL.ByteString where
  _HtmlDocument = to Html.parseLBS
  {-# INLINE _HtmlDocument #-}

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
-- >>> quasiXml ^.. html...name
-- ["br","br"]
--
-- >>> quasiXml ^? xml...name
-- Nothing
html :: AsHtmlDocument t => Fold t Element
html = _HtmlDocument . documentRoot
{-# INLINE html #-}

-- | An alias for 'xml'
root :: AsXmlDocument t => Traversal' t Element
root = xml
{-# INLINE root #-}

-- | A Traversal into XML prolog
prolog :: AsXmlDocument t => Traversal' t Prologue
prolog = _XmlDocument . documentPrologue
{-# INLINE prolog #-}

-- | Fold 'Element' into the XML document
--
-- Convenience function mostly useful because @xml-conduit@ does not
-- provide handy method to convert 'Element' into text. Assumes empty XML prolog
--
-- See also 'render'
--
-- >>> :{
--   let
--     bare l   = (l, Data.Map.empty, [])
--     tag l    = _Element # bare l
--     subtag l = _NodeElement._Element # bare l
--     doc      = tag "root"
--         & elementNodes <>~ [subtag "child1", subtag "child2", subtag "child3"]
--         & elementNodes %~ (subtag "child0" <|)
-- :}
--
-- >>> Data.Text.Lazy.IO.putStr $ doc ^. render
-- <?xml version="1.0" encoding="UTF-8"?><root><child0/><child1/><child2/><child3/></root>
--
-- >>> Data.Text.Lazy.IO.putStr $ doc ^. renderWith (rsPretty .~ True)
-- <?xml version="1.0" encoding="UTF-8"?>
-- <root>
--     <child0/>
--     <child1/>
--     <child2/>
--     <child3/>
-- </root>
renderWith :: AsXmlDocument t => (RenderSettings -> RenderSettings) -> Fold Element t
renderWith r = to (\e -> Document (Prologue [] Nothing []) e []) . re (_XmlDocumentWith id r)
{-# INLINE renderWith #-}

-- | Fold 'Element' into the XML document with the default rendering settings
render :: AsXmlDocument t => Fold Element t
render = renderWith id
{-# INLINE render #-}

-- | A Lens into XML DOCTYPE declaration
--
-- >>> let doc = "<!DOCTYPE foo><root/>" :: TL.Text
--
-- >>> doc ^? prolog.doctype.folded.doctypeName
-- Just "foo"
--
-- >>> doc & prolog.doctype.traverse.doctypeName .~ "moo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE moo><root/>"
--
-- Since @doctype@'s a Lens, it's possible to attach DOCTYPE declaration
-- to an XML document which didn't have it before:
--
-- >>> ("<root/>" :: TL.Text) & prolog.doctype ?~ XML.Doctype "moo" Nothing
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE moo><root/>"
doctype :: Lens' Prologue (Maybe Doctype)
doctype = prologueDoctype
{-# INLINE doctype #-}

-- | A Lens into nodes before XML DOCTYPE declaration
--
-- >>> let doc = "<!--foo--><!DOCTYPE bar><!--baz--><root/>" :: TL.Text
--
-- >>> doc ^? prolog.beforeDoctype.folded.comments
-- Just "foo"
--
-- >>> doc & prolog.beforeDoctype.traverse.comments %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!--FOO--><!DOCTYPE bar><!--baz--><root/>"
beforeDoctype :: Lens' Prologue [Miscellaneous]
beforeDoctype = prologueBefore
{-# INLINE beforeDoctype #-}

-- | A Lens into nodes after XML DOCTYPE declaration
--
-- >>> let doc = "<!--foo--><!DOCTYPE bar><!--baz--><root/>" :: TL.Text
--
-- >>> doc ^? prolog.afterDoctype.folded.comments
-- Just "baz"
--
-- >>> doc & prolog.afterDoctype.traverse.comments %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!--foo--><!DOCTYPE bar><!--BAZ--><root/>"
afterDoctype :: Lens' Prologue [Miscellaneous]
afterDoctype = prologueAfter
{-# INLINE afterDoctype #-}

-- | A Traversal into XML epilog
--
-- >>> let doc = "<root/><!--qux--><?foo bar?><!--quux-->" :: TL.Text
--
-- >>> doc ^.. epilog.folded.comments
-- ["qux","quux"]
--
-- >>> doc ^.. epilog.folded.instructions.target
-- ["foo"]
--
-- >>> doc & epilog .~ []
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root/>"
epilog :: AsXmlDocument t => Traversal' t [Miscellaneous]
epilog = _XmlDocument . documentEpilogue
{-# INLINE epilog #-}

type instance Index Element = Int
type instance IxValue Element = Node

instance Ixed Element where
  ix n = elementNodes . ix n
  {-# INLINE ix #-}

-- | Traverse immediate children
--
-- >>> let doc = "<root><foo>4</foo><foo>7</foo><bar>11</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml...name
-- ["foo","foo","bar"]
--
-- >>> doc & partsOf (root...name) .~ ["boo", "hoo", "moo"]
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
node n = elementNodes . traverse . _NodeElement . named (only n)
{-# INLINE node #-}

-- | Select nodes by name
--
-- >>> let doc = "<root><foo>4</foo><foo>7</foo><bar>11</bar><bar xmlns=\"zap\">28</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml...named (only "foo").name
-- ["foo","foo"]
--
-- >>> doc ^? xml...named (namespace.traverse.only "zap").text
-- Just "28"
--
-- >>> doc ^? xml...named (only "baz").name
-- Nothing
named :: Fold Name a -> Traversal' Element Element
named l = filtered (has (elementName . l))
{-# INLINE named #-}

-- | Traverse node attributes
--
-- >>> let doc = "<root><foo bar=\"baz\" qux=\"zap\"/><foo quux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> doc ^.. xml...attrs.indices (has (name.unpacked.prefixed "qu"))
-- ["zap","xyzzy"]
--
-- >>> doc & xml...attrs %~ Text.toUpper
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"BAZ\" qux=\"ZAP\"/><foo quux=\"XYZZY\"/></root>"
attrs :: IndexedTraversal' Name Element Text
attrs = elementAttributes . itraversed
{-# INLINE attrs #-}

-- | Traverse node attributes with a specific name
--
-- >>> let doc = "<root><foo bar=\"baz\" qux=\"quux\"/><foo qux=\"xyzzy\"/></root>" :: TL.Text
--
-- >>> doc ^.. xml...attr "qux".traverse
-- ["quux","xyzzy"]
--
-- >>> doc ^.. xml...attr "bar"
-- [Just "baz",Nothing]
--
-- >>> doc & xml...attr "qux".traverse %~ Text.reverse
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"baz\" qux=\"xuuq\"/><foo qux=\"yzzyx\"/></root>"
--
-- >>> doc & xml.ix 1._NodeElement.attr "bar" ?~ "bazzy"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><foo bar=\"baz\" qux=\"quux\"/><foo bar=\"bazzy\" qux=\"xyzzy\"/></root>"
attr :: Name -> Lens' Element (Maybe Text)
attr n = elementAttributes . at n
{-# INLINE attr #-}

-- | Select nodes by attributes' values
--
-- >>> let doc = "<root><foo bar=\"baz\">4</foo><foo bar=\"quux\">7</foo><bar bar=\"baz\">11</bar></root>" :: TL.Text
--
-- >>> doc ^.. xml...attributed (ix "bar".only "baz").text
-- ["4","11"]
--
-- >>> doc ^? xml...attributed (folded.to Text.length.only 4).text
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

-- | Anything that has comments
class HasComments t where
  comments :: Traversal' t Text

instance HasComments Element where
  -- | Traverse node comments
  --
  -- >>> let doc = "<root><!-- qux --><foo>bar</foo><!-- quux --></root>" :: TL.Text
  --
  -- >>> doc ^.. xml.comments
  -- [" qux "," quux "]
  --
  -- >>> doc & xml.partsOf comments .~ [" xyz ", " xyzzy "]
  -- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><!-- xyz --><foo>bar</foo><!-- xyzzy --></root>"
  comments = elementNodes . traverse . _NodeComment
  {-# INLINE comments #-}

instance HasComments Miscellaneous where
  -- | Traverse node comments
  comments = _MiscComment
  {-# INLINE comments #-}

-- | Anything that has processing instructions
class HasInstructions t where
  instructions :: Traversal' t Instruction

  -- | Traverse node instructions
  --
  -- >>> let doc = "<root><!-- foo --><?foo bar?><qux/><?xyz xyzzy?><quux/></root>" :: TL.Text
  --
  -- >>> doc ^.. xml.instructions.target
  -- ["foo","xyz"]
  --
  -- >>> doc & xml.instructions.data_ %~ Text.toUpper
  -- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><!-- foo --><?foo BAR?><qux/><?xyz XYZZY?><quux/></root>"
instance HasInstructions Element where
  instructions = elementNodes . traverse . _NodeInstruction
  {-# INLINE instructions #-}

instance HasInstructions Miscellaneous where
  -- | Traverse node instructions
  instructions = _MiscInstruction
  {-# INLINE instructions #-}

-- | Processing instruction target
--
-- >>> let doc = "<root><?foo bar?></root>" :: TL.Text
--
-- >>> doc ^? xml.instructions.target
-- Just "foo"
--
-- >>> doc & xml.instructions.target .~ "boo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><?boo bar?></root>"
target :: Traversal' Instruction Text
target = instructionTarget
{-# INLINE target #-}

-- | Processing instruction data
--
-- >>> let doc = "<root><?foo bar?></root>" :: TL.Text
--
-- >>> doc ^? xml.instructions.data_
-- Just "bar"
--
-- >>> doc & xml.instructions.data_ .~ "hoo"
-- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><?foo hoo?></root>"
data_ :: Traversal' Instruction Text
data_ = instructionData
{-# INLINE data_ #-}

-- | Anything that has a name
class HasName t where
  fullName :: Lens' t Name

instance HasName Name where
  fullName = id
  {-# INLINE fullName #-}

instance HasName Element where
  fullName = elementName
  {-# INLINE fullName #-}

-- | A Lens into node name
--
-- >>> ("<root/>" :: TL.Text) ^. xml.name
-- "root"
--
-- >>> ("<root><foo/><bar/><baz/></root>" :: TL.Text) ^.. xml...name
-- ["foo","bar","baz"]
--
-- >>> ("<root><foo/><bar/><baz></root>" :: TL.Text) & xml.partsOf (plate.name) .~ ["boo", "hoo", "moo"]
-- "<root><foo/><bar/><baz></root>"
name :: HasName t => Lens' t Text
name = fullName . nameLocalName
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
namespace :: HasName t => Lens' t (Maybe Text)
namespace = fullName . nameNamespace
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
prefix :: HasName t => Lens' t (Maybe Text)
prefix = fullName . namePrefix
{-# INLINE prefix #-}

-- | @xml-conduit@ entity resolving exceptions overloading
class AsUnresolvedEntityException t where
  _UnresolvedEntityException :: Prism' t UnresolvedEntityException

instance AsUnresolvedEntityException UnresolvedEntityException where
  _UnresolvedEntityException = id
  {-# INLINE _UnresolvedEntityException #-}

instance AsUnresolvedEntityException SomeException where
  _UnresolvedEntityException = exception
  {-# INLINE _UnresolvedEntityException #-}

-- | @xml-conduit@ general XML exception overloading
class AsXMLException t where
  _XMLException :: Prism' t XMLException

instance AsXMLException XMLException where
  _XMLException = id
  {-# INLINE _XMLException #-}

instance AsXMLException SomeException where
  _XMLException = exception
  {-# INLINE _XMLException #-}

-- | @xml-conduit@ XML parsing exceptions overloading
class AsInvalidEventStream t where
  _InvalidEventStream :: Prism' t InvalidEventStream

instance AsInvalidEventStream InvalidEventStream where
  _InvalidEventStream = id
  {-# INLINE _InvalidEventStream #-}

instance AsInvalidEventStream SomeException where
  _InvalidEventStream = exception
  {-# INLINE _InvalidEventStream #-}

-- | A Prism into 'ContentAfterRoot'
_ContentAfterRoot :: AsInvalidEventStream t => Prism' t EventPos
_ContentAfterRoot = _InvalidEventStream
  . prism' ContentAfterRoot (\s -> case s of ContentAfterRoot e -> Just e; _ -> Nothing)
{-# INLINE _ContentAfterRoot #-}

-- | A Prism into 'MissingRootElement'
_MissingRootElement :: AsInvalidEventStream t => Prism' t ()
_MissingRootElement = _InvalidEventStream
  . prism' (const MissingRootElement) (\s -> case s of MissingRootElement -> Just (); _ -> Nothing)
{-# INLINE _MissingRootElement #-}

-- | A Prism into 'InvalidInlineDoctype'
_InvalidInlineDoctype :: AsInvalidEventStream t => Prism' t EventPos
_InvalidInlineDoctype = _InvalidEventStream
  . prism' InvalidInlineDoctype (\s -> case s of InvalidInlineDoctype e -> Just e; _ -> Nothing)
{-# INLINE _InvalidInlineDoctype #-}

-- | A Prism into 'MissingEndElement'
_MissingEndElement :: AsInvalidEventStream t => Prism' t (Name, Maybe EventPos)
_MissingEndElement = _InvalidEventStream
  . prism' (uncurry MissingEndElement) (\s -> case s of MissingEndElement e p -> Just (e, p); _ -> Nothing)
{-# INLINE _MissingEndElement #-}

-- | A Prism into 'UnterminatedInlineDoctype'
_UnterminatedInlineDoctype :: AsInvalidEventStream t => Prism' t ()
_UnterminatedInlineDoctype = _InvalidEventStream
  . prism' (const UnterminatedInlineDoctype) (\s -> case s of UnterminatedInlineDoctype -> Just (); _ -> Nothing)
{-# INLINE _UnterminatedInlineDoctype #-}
