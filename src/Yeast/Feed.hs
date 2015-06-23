{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

-- This module contains orphan instances for Serialize Text, and
-- Arbitrary (Maybe Text).
{-# OPTIONS_GHC -fno-warn-orphans   #-}

------------------------------------------------------------------------

module Yeast.Feed
  ( Feed
  , FeedF(..)
  , FeedKind(..)
  , Item(..)
  , emptyFeed
  , emptyItem
  , kind
  , title
  , feedHome
  , feedHtml
  , description
  , date
  , items
  , link
  , author
  )
  where

import           Control.Lens             (makeFields)
import qualified Data.ByteString.Base64   as Base64
import           Data.Char                (isSpace)
import           Data.Serialize           (Serialize, get, put, encode,
                                           decode)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8, decodeUtf8)
import           GHC.Generics             (Generic)
import           Servant                  (ToText, toText, FromText,
                                           fromText)
import           Test.QuickCheck          (Arbitrary, Gen, arbitrary,
                                           shrink, elements, frequency)

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative      ((<$>), (<*>), pure)
import           Data.Foldable            (Foldable)
import           Data.Monoid              (mempty)
import           Data.Traversable         (Traversable)
#endif

------------------------------------------------------------------------
-- * Types

type Feed = FeedF [Item]

data FeedF items = Feed
  { _feedFKind        :: !FeedKind
  , _feedFTitle       :: !(Maybe Text)

  -- XXX: Network.URI?
  -- https://stackoverflow.com/questions/30361822/how-to-denote-a-static-haskell-uri-in-code-with-network-uri

  , _feedFFeedHome    :: !(Maybe Text)  -- ^ URL to the feed itself.
  , _feedFFeedHtml    :: !(Maybe Text)  -- ^ URL to the HTML version of
                                        --   the feed.
  , _feedFDescription :: !(Maybe Text)
  , _feedFDate        :: !(Maybe Text)    -- XXX: UTCTime?
  , _feedFItems       :: !items
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

data FeedKind = AtomKind | RSS1Kind | RSS2Kind
  deriving (Bounded, Generic, Enum, Eq, Show)

data Item = Item
  { _itemTitle       :: !(Maybe Text)
  , _itemLink        :: !(Maybe Text)
  , _itemDate        :: !(Maybe Text)
  , _itemAuthor      :: !(Maybe Text)
  , _itemDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Ord, Generic)

------------------------------------------------------------------------
-- * Constructors and lenses

emptyFeed :: FeedKind -> Feed
emptyFeed k = Feed k mempty mempty mempty mempty mempty mempty

emptyItem :: Item
emptyItem = Item Nothing Nothing Nothing Nothing Nothing

makeFields ''FeedF
makeFields ''Item

------------------------------------------------------------------------

instance Serialize a => Serialize (FeedF a)
instance Serialize FeedKind
instance Serialize Item

instance Serialize Text where
  put = put . encodeUtf8
  get = fmap decodeUtf8 get

-- Using the following helper we avoid defining an orphan arbitrary
-- instance for text, and keep the number of nothings down.
arbMText :: Gen (Maybe Text)
arbMText = frequency
  [ (1, pure Nothing)

  -- XXX: Spaces, such as \n, seem to be dropped somewhere while
  -- serving... The following would be better: [ (10, Just . T.strip
  -- . T.pack <$> arbitrary)
  , (10, Just . T.filter (not . isSpace) . T.pack <$> arbitrary)
  ]

shrinkMText :: Maybe Text -> [Maybe Text]
shrinkMText Nothing  = []
shrinkMText (Just t) = [Nothing] ++
  [Just (T.pack s) | s <- shrink (T.unpack t)]

instance Arbitrary (Maybe Text) where
  arbitrary = arbMText
  shrink    = shrinkMText

-- XXX: Shrink the items as well.
instance Arbitrary a => Arbitrary (FeedF a) where
  arbitrary = Feed <$> arbitrary <*> arbMText <*> pure Nothing <*> arbMText
                   <*> arbMText  <*> arbMText <*> arbitrary
  shrink (Feed k t ho ht de da is) =
    [ Feed k t' ho' ht' de' da' is
    | (t', ho', ht', de', da') <- shrink (t, ho, ht, de, da)
    ]

instance Arbitrary FeedKind where
  arbitrary = elements $ enumFrom minBound

instance Arbitrary Item where
  arbitrary = Item <$> arbMText <*> arbMText <*> pure Nothing
                   <*> arbMText <*> arbMText
  shrink (Item t l da a de) =
    [ Item t' l' da' a' de'
    | (t', l', da', a', de') <- shrink (t, l, da, a, de)
    ]

instance ToText Feed where
  toText = decodeUtf8 . Base64.encode . encode

instance ToText Item where
  toText = decodeUtf8 . Base64.encode . encode

instance FromText Feed where
  fromText
    = either (const Nothing) Just
    . decode . Base64.decodeLenient . encodeUtf8

instance FromText Item where
  fromText
    = either (const Nothing) Just
    . decode . Base64.decodeLenient . encodeUtf8
