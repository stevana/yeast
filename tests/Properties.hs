module Properties
  (properties)
  where

import           Control.Lens               ((^.), (.~), (?~), (%~), (&),
                                             mapped, to)
import           Control.Monad.Trans.Either (runEitherT)
import           Test.Tasty.QuickCheck      (Property, testProperty)
import           Data.Serialize             (encode, decode)
import qualified Data.Text                  as T
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as L
import           Data.Text.Lens             (unpacked)
import           Servant                    (toText, fromText)
import           Servant.Client             (showBaseUrl)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.QuickCheck.Monadic    (monadicIO, run, assert)
import qualified Text.Feed.Constructor      as Old
import qualified Text.Feed.Export           as Old
import qualified Text.Feed.Import           as Old
import qualified Text.Feed.Query            as Old
import qualified Text.Feed.Types            as Old
import qualified Text.XML.Light.Output      as XML

import           Yeast.Feed
import           Yeast.Fetch
import           Yeast.Parse
import           Yeast.Render
import           Yeast.Serve

------------------------------------------------------------------------
-- * Properties

prop_serialize :: Feed -> Bool
prop_serialize f = decode (encode f) == Right f

prop_toFromText :: Feed -> Bool
prop_toFromText f = fromText (toText f) == Just f

prop_addFeed :: Feed -> Property
prop_addFeed f = monadicIO $ do
  run $ withServer [] $ do
    ei <- runEitherT $ addFeed f
    case ei of
      Left err -> fail $ show err
      Right _  -> return ()

prop_parseRender :: Feed -> Bool
prop_parseRender f
  = either (const False) ((==) f)
  . parseText
  . render (def { rsPretty = True })
  $ f

prop_roundTrip :: Feed -> Property
prop_roundTrip f = monadicIO $ do
  ef' <- run $ withServer [] $ do
    ei <- runEitherT $ addFeed f
    case ei of
      Left err -> fail  $ show err
      Right i  -> fetch $ showBaseUrl baseUrl ++ "/feed/view/" ++ show i
  case ef' of
    Left  err -> fail $ show err
    Right f'  -> assert $ f == f'

-- | The output of our renderer can be parsed by the
-- old feed library's parser.
prop_oldParseRender :: Feed -> Bool
prop_oldParseRender f0
  = maybe False ((==) f0 . fromOldFeed)
  . parseOldFeed
  . render (def { rsPretty = True })
  $ f0
  where
  parseOldFeed :: Text -> Maybe Old.Feed
  parseOldFeed = Old.parseFeedString . L.unpack

  fromOldFeed :: Old.Feed -> Feed
  fromOldFeed f = emptyFeed (fromOldKind (Old.getFeedKind f))
    & title       ?~ T.pack (Old.getFeedTitle f)
    & feedHome    .~ fmap T.pack (Old.getFeedHome f)
    & feedHtml    .~ fmap T.pack (Old.getFeedHTML f)
    & description .~ fmap T.pack (Old.getFeedDescription f)
    & date        .~ fmap T.pack (Old.getFeedDate f)
    & items       .~ map fromOldItem (Old.getFeedItems f)
    where
    fromOldKind :: Old.FeedKind -> FeedKind
    fromOldKind (Old.RDFKind _) = RSS1Kind
    fromOldKind (Old.RSSKind _) = RSS2Kind
    fromOldKind Old.AtomKind    = AtomKind

    fromOldItem :: Old.Item -> Item
    fromOldItem i = emptyItem
      & title       .~ fmap T.pack (Old.getItemTitle i)
      & link        .~ fmap T.pack (Old.getItemLink i)
      & date        .~ fmap T.pack (Old.getItemDate i)
      & author      .~ fmap T.pack (Old.getItemAuthor i)
      & description .~ fmap T.pack (Old.getItemDescription i)

-- | The output of the old feed library's renderer can be parsed.
prop_parseOldRender :: Feed -> Bool
prop_parseOldRender f0
  = either (const False) ((==) f0)
  . parseText
  . renderOldFeed
  . toOldFeed
  $ f0
  where
  toOldFeed :: Feed -> Old.Feed
  toOldFeed f
    = Old.withFeedTitle       (f^.title      .traverse.unpacked)
    . Old.withFeedHome        (f^.feedHome   .traverse.unpacked)
    . Old.withFeedHTML        (f^.feedHtml   .traverse.unpacked)
    . Old.withFeedDescription (f^.description.traverse.unpacked)
    . Old.withFeedDate        (f^.date       .traverse.unpacked)
    . Old.withFeedItems       (f^.items & mapped %~ toOldItem)
    $ Old.newFeed             (f^.kind.to toOldKind)
    where
    toOldKind :: FeedKind -> Old.FeedKind
    toOldKind RSS1Kind = Old.RDFKind Nothing
    toOldKind RSS2Kind = Old.RSSKind Nothing
    toOldKind AtomKind = Old.AtomKind

    toOldItem :: Item -> Old.Item
    toOldItem i
      = Old.withItemTitle       (i^.title      .traverse.unpacked)
      . Old.withItemLink        (i^.link       .traverse.unpacked)
      . Old.withItemDate        (i^.date       .traverse.unpacked)
      . Old.withItemAuthor      (i^.author     .traverse.unpacked)
      . Old.withItemDescription (i^.description.traverse.unpacked)
      $ Old.newItem             (f^.kind.to toOldKind)

  renderOldFeed :: Old.Feed -> Text
  renderOldFeed = L.pack . XML.ppElement . Old.xmlFeed

-- XXX: Test renderer against external feed validator?
-- https://validator.w3.org/feed/

------------------------------------------------------------------------

-- XXX: Use Test.Tasty.TH?

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "serialize"        prop_serialize
  , testProperty "{To,From}Text"    prop_toFromText
  , testProperty "add feed"         prop_addFeed
  , testProperty "parseRender"      prop_parseRender
  , testProperty "round trip"       prop_roundTrip
  , testProperty "old parse render" prop_oldParseRender
  , testProperty "parse old render" prop_parseOldRender
  ]
