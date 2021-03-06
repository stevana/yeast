{-# LANGUAGE DeriveAnyClass         #-}
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

-- This module contains orphan instances for Serialize Text,
-- Arbitrary (Maybe Text), and Arbitrary (,,,,,).
{-# OPTIONS_GHC -fno-warn-orphans   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

------------------------------------------------------------------------
-- |
-- Module      :  Yeast.Feed
-- Copyright   :  (c) 2015-2016 Stevan Andjelkovic
-- License     :  ISC (see the file LICENSE)
-- Maintainer  :  Stevan Andjelkovic
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains the datatype capturing the notion of a news
-- feed, together with its constructors and lenses.
--
-- RSS1, RSS2 and Atom feeds will all be parsed into the feed datatype,
-- so that they can be manipulated in a uniform way.
--
------------------------------------------------------------------------

module Yeast.Feed (

  -- * Types
  -- $types
    Feed
  , FeedF(..)
  , FeedKind(..)
  , Item(..)

  -- * Constructors
  -- $constructors
  , emptyFeed
  , emptyItem

  -- * Lenses
  -- $lenses

  -- ** Feed lenses

  , kind
  -- | A lens for the kind of a feed.

  , title
  -- | A lens for the title, this one is overloaded to work for both
  -- feeds and items (since both have a title).

  , feedHome
  -- | A lens for the URL of the feed.

  , feedHtml
  -- | A lens for the HTML version of the feed.

  , description
  -- | A lens for the description of a feed or item.

  , date
  -- | A lens for the date of a feed or item.

  , items
  -- | A lens for the items of a feed.

  -- ** Item lenses
  -- __Note:__ that some of the already exported feed lenses
  -- are overloaded to work with items as well, e.g. 'title',
  -- 'description', and 'date'.

  , link
  -- | A lens for the link of an item.

  , author
  -- | A lens for the author of an item.
  )
  where

import           Control.Lens              (makeFields)
import           Data.Char                 (isSpace)
import           Data.Serialize            (Serialize, get, put)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           GHC.Generics              (Generic)
import           Servant.API               (FromHttpApiData, ToHttpApiData)
import           Test.QuickCheck           (Arbitrary, Gen, arbitrary, elements,
                                            frequency, shrink)
import           Test.QuickCheck.Instances ()

------------------------------------------------------------------------
-- $types
-- Datatypes for feeds and their items.

-- | The feed datatype.
type Feed = FeedF [Item]

-- | The feed datatype parametrised by some notion of feed items.
data FeedF items = Feed
  { _feedFKind        :: !FeedKind
  , _feedFTitle       :: !(Maybe Text)

  -- XXX: Use Network.URI?
  -- https://stackoverflow.com/questions/30361822/how-to-denote-a-static-haskell-uri-in-code-with-network-uri
  , _feedFFeedHome    :: !(Maybe Text)  -- ^ URL to the feed itself.
  , _feedFFeedHtml    :: !(Maybe Text)  -- ^ URL to the HTML version of
                                        --   the feed.
  , _feedFDescription :: !(Maybe Text)
  , _feedFDate        :: !(Maybe Text)    -- XXX: UTCTime?
  , _feedFItems       :: !items
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, ToHttpApiData, FromHttpApiData)

-- | The feed kind datatype.
data FeedKind = AtomKind | RSS1Kind | RSS2Kind
  deriving (Bounded, Generic, Enum, Eq, Show, ToHttpApiData, FromHttpApiData)

-- | The item datatype, represents an item entry in the feed.
data Item = Item
  { _itemTitle       :: !(Maybe Text)
  , _itemLink        :: !(Maybe Text)
  , _itemDate        :: !(Maybe Text)
  , _itemAuthor      :: !(Maybe Text)
  , _itemDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Ord, Generic, ToHttpApiData, FromHttpApiData)

deriving instance ToHttpApiData   [Item]
deriving instance FromHttpApiData [Item]

------------------------------------------------------------------------
-- $constructors
-- Constructors for feeds and their items.

-- | Empty feed.
emptyFeed :: Monoid m => FeedKind -> FeedF m
emptyFeed k = Feed k Nothing Nothing Nothing Nothing Nothing mempty

-- | Empty item.
emptyItem :: Item
emptyItem = Item Nothing Nothing Nothing Nothing Nothing

------------------------------------------------------------------------
-- $lenses
-- Lenses for feeds and items.
--
-- __Note:__ These are derived using 'makeFields' from
-- "Control.Lens.TH", which drops the @_datatypeName@ prefix of the
-- fields and allows us to overload the lenses via the @HasFieldName@
-- type classes.

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

instance Arbitrary a => Arbitrary (FeedF a) where
  arbitrary = do
    k <- arbitrary
    Feed <$> pure k
         <*> arbMText
         <*> case k of
               -- Only Atom feeds have a feed home.
               AtomKind -> arbMText
               _        -> pure Nothing
         <*> arbMText
         <*> arbMText
         <*> case k of
               -- RSS2 feeds do not have a date.
               RSS2Kind -> pure Nothing
               _        -> arbMText
         <*> arbitrary

  shrink (Feed k t ho ht de da is) =
    [ Feed k t' ho' ht' de' da' is'
    | (t', ho', ht', de', da', is') <- shrink (t, ho, ht, de, da, is)
    ]

instance (Arbitrary a, Arbitrary b, Arbitrary c,
          Arbitrary d, Arbitrary e, Arbitrary f) =>
            Arbitrary (a, b, c, d, e, f) where
  arbitrary = (,,,,,) <$> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (u, v, w, x, y, z) =
    [ (u', v', w', x', y', z')
    | (u', (v', (w', (x', (y', z'))))) <- shrink (u, (v, (w, (x, (y, z)))))
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
