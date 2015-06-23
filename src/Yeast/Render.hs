{-# LANGUAGE OverloadedStrings #-}

module Yeast.Render
  ( render
  , RenderSettings(..)
  , def
  )
  where

import qualified Data.Map       as M
import           Data.Maybe     (isJust)
import           Data.Text      (Text)
import qualified Data.Text.Lazy as L
import           Text.XML       (Document(Document), Element(Element),
                                 Name(Name), Node(NodeElement, NodeContent),
                                 Prologue(Prologue), RenderSettings, def)
import qualified Text.XML       as XML
import           Text.XML.Lens  ((^.), (.~), (&), root, nodes, attrs,
                                 _Element)

import           Yeast.Feed

------------------------------------------------------------------------
-- * Rendering

render :: RenderSettings -> Feed -> L.Text
render rs = XML.renderText rs . toXML

------------------------------------------------------------------------

emptyDocument :: FeedKind -> Document
emptyDocument k = Document (Prologue [] Nothing []) rootEl []
  where
  rootEl :: Element
  rootEl = case k of
    RSS1Kind -> Element "{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF"
                  M.empty []
    RSS2Kind -> Element "rss" (M.fromList [("version", "2.0")]) []
    AtomKind -> Element "{http://www.w3.org/2005/Atom}feed" M.empty []

node :: Text -> [Node] -> Node
node t ns = NodeElement $ Element
  (Name t Nothing Nothing) M.empty ns

leaf :: Text -> Maybe Text -> [Node]
leaf _ Nothing   = []
leaf t (Just t') = [node t [NodeContent t']]

attrLeaf :: Text -> [(Text, Maybe Text)] -> [Node]
attrLeaf t0 as = case as' of
  [] -> []
  _  -> [node t0 [] & _Element.attrs .~ M.fromList as']
  where
  as' :: [(Name, Text)]
  as' = map (\(t, Just t') -> (Name t Nothing Nothing, t'))
      $ filter (isJust . snd) as

toXML :: Feed -> Document
toXML f = emptyDocument (f^.kind) & root.nodes .~ case f^.kind of

    RSS1Kind ->
      node "channel"
        (  leaf "title"       (f^.title)
        ++ leaf "link"        (f^.feedHtml)
        ++ leaf "description" (f^.description)
        ++ leaf "date"        (f^.date))
      : flip map (f^.items) (\i ->
          node "item"
            $  leaf "title"       (i^.title)
            ++ leaf "link"        (i^.link)
            ++ leaf "date"        (i^.date)
            ++ leaf "dc:creator"  (i^.author)
            ++ leaf "description" (i^.description)
            )

    RSS2Kind -> [
      node "channel"
        (  leaf "title"       (f^.title)
        ++ leaf "link"        (f^.feedHtml)
        ++ leaf "description" (f^.description)
        ++ flip map (f^.items) (\i ->
             node "item"
               $  leaf "title"       (i^.title)
               ++ leaf "link"        (i^.link)
               ++ leaf "pubDate"     (i^.date)
               ++ leaf "author"      (i^.author)
               ++ leaf "description" (i^.description))
        )
      ]

    AtomKind ->
         leaf     "title"    (f^.title)
      ++ leaf     "subtitle" (f^.description)
      ++ attrLeaf "link"
           [ ("type", Just "text/html")
           , ("rel",  Just "alternative")
           , ("href", f^.feedHtml)
           ]
      ++ attrLeaf "link"
           [ ("type", Just "application/atom+xml")
           , ("rel",  Just "self")
           , ("href", f^.feedHome)
           ]
      ++ leaf "updated" (f^.date)
      ++ flip map (f^.items) (\i ->
           node "entry"
             $  leaf "title" (i^.title)
             ++ attrLeaf "link"
                  [ ("type", Just "text/html")
                  , ("rel",  Just "alternative")
                  , ("href", i^.link)
                  ]
             ++ leaf "updated" (i^.date)
             ++ [node "author" (leaf "name" (i^.author))]
             ++ leaf "content" (i^.description)
         )
