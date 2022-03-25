{-|
  Description: Tools to parse and transform the Docbook descriptions for, e.g. services, into GTK pango markup (see https://developer.gnome.org/pango/stable/pango-Markup.html)

Tools to parse and transform the Docbook descriptions for, e.g. services, into GTK pango markup (see https://developer.gnome.org/pango/stable/pango-Markup.html)
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Docbook
  ( parseDocbook
  , docbookToPango
  )
where

import           Control.Lens    (folded, plate, to, view, (^.))
import           Data.Default    (def)
import           Data.Text       (Text, replace)
import           Data.Text.Lazy  (fromStrict)
import           NixManager.Util (Endo, TextualError, addToError,
                                  fromExceptionEither, surroundSimple)
import           Text.XML        (Document, Element,
                                  Node (NodeComment, NodeContent, NodeElement, NodeInstruction),
                                  parseText)
import           Text.XML.Lens   (attr, localName, named, nodes, root, text)

-- | Parse a docbook string into a valid 'Document' (or return an error)
parseDocbook :: Text -> TextualError Document
parseDocbook =
  addToError "error parsing documentation: "
    . fromExceptionEither
    . parseText def
    . fromStrict
    . surroundSimple "root" -- the XML parser needs a root element

-- | Stupidly replace HTML entities by their ampersandish equivalents (GTK will complain otherwise). This function possibly misses entities, I haven't look them all up.
replaceEntities :: Endo Text
replaceEntities =
  replace "<" "&lt;" . replace ">" "&gt;" . replace "\"" "&quot;"

-- | Convert Docbook XML to Pango XML
docbookToPango :: Document -> Text
docbookToPango rootNode = rootNode ^. root . nodes . folded . to nodeToPango
 where
  nodeToPango :: Node -> Text
  nodeToPango (NodeElement     e) = nodeElementToPango (e ^. localName) e
  nodeToPango (NodeContent     t) = replaceEntities t
  nodeToPango (NodeInstruction _) = ""
  nodeToPango (NodeComment     _) = ""
  makeTt = surroundSimple "tt" . replaceEntities . view text
  nodeElementToPango :: Text -> Element -> Text
  nodeElementToPango "link"           e = e ^. attr "href"
  nodeElementToPango "filename"       e = makeTt e
  nodeElementToPango "literal"        e = makeTt e
  nodeElementToPango "command"        e = makeTt e
  nodeElementToPango "option"         e = makeTt e
  nodeElementToPango "code"           e = makeTt e
  nodeElementToPango "programlisting" e = makeTt e
  nodeElementToPango "varname"        e = makeTt e
  nodeElementToPango "citerefentry" e =
    "man "
      <> (e ^. plate . named "manvolnum" . text)
      <> " "
      <> (e ^. plate . named "refentrytitle" . text)
  nodeElementToPango _ e = e ^. nodes . folded . to nodeToPango
