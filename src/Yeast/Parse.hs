{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings #-}

module Yeast.Parse
  ( ParseError(..)
  , parseFile
  , parseLBS
  , parseText
  )
  where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative     ((<$>))
#endif
import           Control.Applicative     ((<|>))
import           Control.DeepSeq         (deepseq)
import           Control.Exception       (SomeException)
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (decodeLatin1, decodeUtf8')
import qualified Text.XML                as XML
import           Text.XML.Lens           (Document, Element,
                                          (^?), (^..), (&), (.~), (%~),
                                          (./), root, el, ell, entire,
                                          text, localName, attributeIs,
                                          attr, to, mapped, bimap)

import           Yeast.Feed

------------------------------------------------------------------------

data ParseError
  = UnknownFeedKind
  | XMLConduitError SomeException
  deriving Show

parseFile :: FilePath -> IO (Either ParseError Feed)
parseFile fp = parseLBS <$> LBS.readFile fp

parseLBS :: ByteString -> Either ParseError Feed
parseLBS bs =
  parseText $ either (const (decodeLatin1 bs)) id (decodeUtf8' bs)

parseText :: Text -> Either ParseError Feed
parseText txt = do
  doc <- bimap XMLConduitError id $ XML.parseText XML.def txt
  k   <- parseKind doc
  deepseq doc $ return $ fromXML doc k
  where
  parseKind :: Document -> Either ParseError FeedKind
  parseKind doc = case doc^?root.localName of
    Just "RDF"  -> return RSS1Kind
    Just "rss"  -> return RSS2Kind
    Just "feed" -> return AtomKind
    _           -> Left   UnknownFeedKind

  fromXML :: Document -> FeedKind -> Feed
  fromXML doc k = emptyFeed k

    & title       .~ case k of
        RSS1Kind -> doc^?root./ell "channel"./ell "title".text.to T.strip
        RSS2Kind -> doc^?root./el  "channel"./el  "title".text.to T.strip
        AtomKind -> doc^?root./ell                "title".text.to T.strip

    & feedHome    .~ case k of
        RSS1Kind -> Nothing
        RSS2Kind -> Nothing
        AtomKind -> doc^?root.ell "feed"./ell "link".
                      attributeIs "type" "application/atom+xml".
                        attr "href".to T.strip

    & feedHtml    .~ case k of
        RSS1Kind -> doc^?root./ell "channel"./ell "link".text.to T.strip
        RSS2Kind -> doc^?root./ell "channel"./ell "link".text.to T.strip
        AtomKind -> doc^?root.ell "feed"./ell "link".
                      attributeIs "type" "text/html".attr "href".to T.strip

    & description .~ case k of
        RSS1Kind -> doc^?root.entire.ell "channel"./
                      ell "description".text.to T.strip
        RSS2Kind -> doc^?root.entire.ell "channel"./
                      ell "description".text.to T.strip
        AtomKind -> doc^?root.ell "feed".ell "subtitle".text.to T.strip

    & date        .~ case k of
        RSS1Kind -> doc^?root.entire./ell "date".text.to T.strip
        RSS2Kind -> Nothing
        AtomKind -> doc^?root.entire.ell "updated".text.to T.strip

    & items       .~ case k of
        RSS1Kind -> doc^..root./ell "item" & mapped %~ parseItem
        RSS2Kind ->
          doc^..root./el "channel"./el "item" & mapped %~ parseItem
        AtomKind -> doc^..root./ell "entry" & mapped %~ parseItem
    where
    parseItem :: Element -> Item
    parseItem e = emptyItem
      & title       .~ case k of
          RSS1Kind -> e^?entire.ell "title".text.to T.strip
          RSS2Kind -> e^?entire.el  "title".text.to T.strip
          AtomKind -> e^?entire.ell "title".text.to T.strip
      & link        .~ case k of
          RSS1Kind -> e^?entire.ell "link".text.to T.strip
          RSS2Kind -> e^?entire.el  "link".text.to T.strip
          AtomKind -> e^?entire.ell "link".attr "href".to T.strip
      & date        .~ case k of
          RSS1Kind -> Nothing
          RSS2Kind -> e^?entire.el  "pubDate".text
          AtomKind -> e^?entire.ell "published".text <|>
                      e^?entire.ell "updated".text
      & author      .~ case k of
          RSS1Kind -> e^?entire.ell "creator".text.to T.strip
          RSS2Kind -> e^?entire.el  "author".text.to T.strip
          AtomKind -> e^?entire.ell "author"./ell "name".text.to T.strip
      & description .~ case k of
          RSS1Kind -> e^?entire.ell "description".text.to T.strip
          RSS2Kind -> e^?entire.el  "description".text.to T.strip
          AtomKind -> e^?entire.ell "summary".text.to T.strip <>
                      e^?entire.ell "content".text.to T.strip
