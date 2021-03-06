module Properties
  (properties)
  where

import           Test.Tasty.QuickCheck      (testProperty)
import           Data.Serialize             (encode, decode)
import           Test.Tasty                 (TestTree, testGroup)

import           Yeast.Feed
import           Yeast.Parse
import           Yeast.Render

------------------------------------------------------------------------
-- * Properties

prop_serialize :: Feed -> Bool
prop_serialize f = decode (encode f) == Right f

prop_parseRender :: Feed -> Bool
prop_parseRender f
  = either (const False) ((==) f)
  . parseText
  . renderText (def { rsPretty = True })
  $ f

-- XXX: Test renderer against external feed validator?
-- https://validator.w3.org/feed/

------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "serialize"        prop_serialize
  , testProperty "parseRender"      prop_parseRender
  ]
