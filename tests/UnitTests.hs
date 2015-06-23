{-# LANGUAGE OverloadedStrings #-}

module UnitTests
  (unitTests)
  where

import           Control.Lens               ((^.), (.~), (?~), (&), ix, to)
import           Control.Monad              (forM_)
import qualified Data.Text                  as T
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertEqual,
                                             assertFailure, testCase)

import           Yeast.Feed
import           Yeast.Serve

------------------------------------------------------------------------
-- * Helpers

assertFeed
  :: Feed       -- ^ Expected
  -> Feed       -- ^ Actual
  -> Assertion
assertFeed f g = do
  assertEqual "Feed kind"        (f^.kind)             (g^.kind)
  assertEqual "Feed title"       (f^.title)            (g^.title)
  assertEqual "Feed home"        (f^.feedHome)         (g^.feedHome)
  assertEqual "Feed HTML"        (f^.feedHtml)         (g^.feedHtml)
  assertEqual "Feed description" (f^.description)      (g^.description)
  assertEqual "Feed date"        (f^.date)             (g^.date)

  assertEqual "Items length"     (f^.items.to length)  (g^.items.to length)

  forM_ [0 .. f^.items.to length - 1] $ \n -> do

    assertEqual ("Item title "       ++ show n)
      (f^.items.ix n.title)       (g^.items.ix n.title)
    assertEqual ("Item link "        ++ show n)
      (f^.items.ix n.link)        (g^.items.ix n.link)
    assertEqual ("Item date "        ++ show n)
      (f^.items.ix n.date)        (g^.items.ix n.date)
    assertEqual ("Item author "      ++ show n)
      (f^.items.ix n.author)      (g^.items.ix n.author)
    assertEqual ("Item description " ++ show n)
      (f^.items.ix n.description) (g^.items.ix n.description)

------------------------------------------------------------------------
-- * Unit tests

test_rss1ArXiv :: IO ()
test_rss1ArXiv =
  withFileFeed "tests/files/arxiv-cs-pl.rss1" assertFailure $ \f ->
    flip assertFeed f $ emptyFeed RSS1Kind
      & title       ?~ "cs.PL updates on arXiv.org"
      & feedHome    .~ Nothing
      & feedHtml    ?~ "http://arxiv.org/"
      & description ?~ T.concat
          [ "Computer Science -- Programming Languages (cs.PL) updates"
          , " on the arXiv.org e-print archive"
          ]
      & date        ?~ "2015-06-11T20:30:00-05:00"
      & items       .~
          [ emptyItem
              & title       ?~ T.concat
                  [ "OMP2MPI: Automatic MPI code generation from OpenMP"
                  , " programs. (arXiv:1502.02921v2 [cs.DC] UPDATED)"
                  ]
              & link        ?~ "http://arxiv.org/abs/1502.02921"
              & date        .~ Nothing
              & author      ?~ T.concat
                  [ "<a href=\"http://arxiv.org/find/cs/1/au:+Saa_Garriga"
                  , "_A/0/1/0/all/0/1\">Albert Saa-Garriga</a>, <a href"
                  , "=\"http://arxiv.org/find/cs/1/au:+Castells_Rufas_D/"
                  , "0/1/0/all/0/1\">David Castells-Rufas</a>, <a href"
                  , "=\"http://arxiv.org/find/cs/1/au:+Carrabina_J/0/1/"
                  , "0/all/0/1\">Jordi Carrabina</a>"
                  ]
              & description ?~ T.concat
                  [ "<p>In this paper, we present OMP2MPI a tool that "
                  , "generates automatically MPI\nsource code from "
                  , "OpenMP. With this transformation the original "
                  , "program can be\nadapted to be able to exploit a "
                  , "larger number of processors by surpassing the\n"
                  , "limits of the node level on large HPC clusters. "
                  , "The transformation can also be\nuseful to adapt "
                  , "the source code to execute in distributed memory "
                  , "many-cores\nwith message passing support. In "
                  , "addition, the resulting MPI code can be used\nas "
                  , "an starting point that still can be further "
                  , "optimized by software engineers.\nThe "
                  , "transformation process is focused on detecting "
                  , "OpenMP parallel loops and\ndistributing them in a "
                  , "master/worker pattern. A set of micro-benchmarks "
                  , "have\nbeen used to verify the correctness of the "
                  , "the transformation and to measure\nthe resulting "
                  , "performance. Surprisingly not only the "
                  , "automatically generated\ncode is correct by "
                  , "construction, but also it often performs faster "
                  , "even when\nexecuted with MPI.\n</p>"
                  ]
          , emptyItem
              & title       ?~ T.concat
                  [ "Node.DPWS: High performance and scalable Web "
                  , "Services for the IoT. (arXiv:1503.01398v2 [cs.NI] "
                  , "UPDATED)"
                  ]
              & link        ?~ "http://arxiv.org/abs/1503.01398"
              & date        .~ Nothing
              & author      ?~ T.concat
                  [ "<a href=\"http://arxiv.org/find/cs/1/au:+Fysarakis_K/0"
                  , "/1/0/all/0/1\">Konstantinos Fysarakis</a> (1), <a href"
                  , "=\"http://arxiv.org/find/cs/1/au:+Mylonakis_D/0/1/0/al"
                  , "l/0/1\">Damianos Mylonakis</a> (2), <a href=\"http://"
                  , "arxiv.org/find/cs/1/au:+Manifavas_C/0/1/0/all/0/1\">"
                  , "Charalampos Manifavas</a> (3), <a href=\"http://arxiv"
                  , ".org/find/cs/1/au:+Papaefstathiou_I/0/1/0/all/0/1\">"
                  , "Ioannis Papaefstathiou</a> (1) ((1) Dept. of "
                  , "Electronic &amp; Computer Engineering, Technical "
                  , "University of Crete, Greece, (2) Dept. of Computer "
                  , "Science, University of Crete, Greece, (3) Dept. of "
                  , "Informatics Engineering, Technological Educational "
                  , "Institute of Crete, Greece)"
                  ]
              & description ?~ T.concat
                  [ "<p>Interconnected computing systems, in various "
                  , "forms, are expected to permeate\nour lives, "
                  , "realizing the vision of the Internet of Things "
                  , "(IoT) and allowing us\nto enjoy novel, enhanced "
                  , "services that promise to improve our everyday "
                  , "lives.\nNevertheless, this new reality also "
                  , "introduces significant challenges in terms\nof "
                  , "performance, scaling, usability and "
                  , "interoperability. Leveraging the\nbenefits of "
                  , "Service Oriented Architectures (SOAs) can help "
                  , "alleviate many of\nthe issues that developers, "
                  , "implementers and end-users have to face in the\n"
                  , "context of the IoT. This work presents Node.DPWS, "
                  , "a novel implementation of the\nDevices Profile "
                  , "for Web Services (DPWS) based on the Node.js "
                  , "platform.\nNode.DPWS can be used to deploy "
                  , "lightweight, efficient and scalable Web\nServices "
                  , "over heterogeneous nodes, including devices with "
                  , "limited resources.\nThe performance of the "
                  , "presented work is evaluated on typical embedded "
                  , "devices,\nincluding comparisons with "
                  , "implementations created using alternative "
                  , "DPWS\ntoolkits.\n</p>"
                  ]
          ]

test_rss2ConstructiveNews :: IO ()
test_rss2ConstructiveNews =
  withFileFeed "tests/files/constructive-news.rss2" assertFailure $ \f -> do
    flip assertFeed f $ emptyFeed RSS2Kind
      & title       ?~ "constructivenews"
      & feedHome    .~ Nothing
      & feedHtml    ?~ "https://groups.google.com/d/forum/constructivenews"
      & description ?~ T.concat
          [ "News on constructive mathematics such as publication of "
          , "new papers and discussion including topics: Bishop "
          , "constructive mathematics, intuitionism, computability, "
          , "realizability, type theory, topos theory, formal "
          , "topology, &nbsp;locale theory, applications, and others."
          ]
      & date        .~ Nothing
      & items       .~
          [ emptyItem
              & title       ?~ T.concat
                  [ "FW: Young Researchers in Mathematics Conference "
                  , "2015 - Oxford"
                  ]
              & link        ?~ T.concat
                  [ "https://groups.google.com/d/msg/constructivenews/"
                  , "KUvEVQWEKDw/tsnpm_BRbiYJ"
                  ]
              & date        ?~ "Tue, 02 Jun 2015 15:20:15 UTC"
              & author      ?~ "Kreinovich, Vladik"
              & description ?~ T.concat
                  [ "FYI -----Original Message----- Young Researchers "
                  , "in Mathematics Conference 2015 - Oxford "
                  , "http://yrm2015.co.uk Monday, August 17, 2015 to "
                  , "Thursday, August 20, 2015 Conference poster: "
                  , "http://yrm2015.co.uk/poster The YRM2015 conference"
                  , " is primarily intended for PhD students in "
                  , "Mathematics an"
                  ]
          , emptyItem
              & title       ?~ T.concat
                  [ "Continuity, Computability, Constructivity 2015; "
                  , "second call for submission"
                  ]
              & link        ?~ T.concat
                  [ "https://groups.google.com/d/msg/constructivenews/"
                  , "0U7QOmYQ6FY/ZUefo2WhRmgJ"
                  ]
              & date        ?~ "Thu, 21 May 2015 12:34:56 UTC"
              & author      ?~ "spreen"
              & description ?~ T.concat
                  [ "*Continuity, Computability, Constructivity \8211 "
                  , "From Logic to Algorithms* *(CCC 2015)* * Schloss "
                  , "Aspenstein, Kochel am See (near Munich)* *14-18 "
                  , "September 2015* * Second call for papers* "
                  , "*http://www.cs.swan.ac.uk/ccc2015/* CCC is a "
                  , "workshop series bringing together researchers "
                  , "from real analysis, com"
                  ]
          ]

test_atomAgdaCommits :: IO ()
test_atomAgdaCommits =
  withFileFeed "tests/files/agda-commits.atom" assertFailure $ \f -> do
    flip assertFeed f $ emptyFeed AtomKind
      & title       ?~ "Recent Commits to agda:master"
      & feedHome    ?~ "https://github.com/agda/agda/commits/master.atom"
      & feedHtml    ?~ "https://github.com/agda/agda/commits/master"
      & description .~ Nothing
      & date        ?~ "2015-06-13T07:43:29-05:00"
      & items       .~
          [ emptyItem
              & title       ?~ "Merge agda-maint"
              & link        ?~ T.concat
                  [ "https://github.com/agda/agda/commit/"
                  , "7aeff4cb610833bf8b4d9ea02416f680cb3b103f"
                  ]
              & date        ?~ "2015-06-13T07:43:29-05:00"
              & author      ?~ "asr"
              & description ?~ T.concat
                  [ "<pre style='white-space:pre-wrap;width:81ex'>"
                  , "Merge agda-maint</pre>"
                  ]
          , emptyItem
              & title       ?~ T.concat
                  [ "[ Issue 1460 ] Removed multiple LANGUAGE pragmas "
                  , "(TypeChecking/Substitute.hs)."
                  ]
              & link        ?~ T.concat
                  [ "https://github.com/agda/agda/commit/"
                  , "c1f3d15930cfdba193e52280227cbef86049c397"
                  ]
              & date        ?~ "2015-06-13T07:42:35-05:00"
              & author      ?~ "asr"
              & description ?~ T.concat
                  [ "<pre style='white-space:pre-wrap;width:81ex'>"
                  , "[ Issue 1460 ] Removed multiple LANGUAGE pragmas "
                  , "(TypeChecking/Substitute.hs).\n\n"
                  , "See error in https://travis-ci.org/agda/agda/jobs"
                  , "/66654433.</pre>"
                  ]
          ]

------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "RSS1 arXiv"             test_rss1ArXiv
  , testCase "RSS2 constructive news" test_rss2ConstructiveNews
  , testCase "Atom Agda commits"      test_atomAgdaCommits
  ]
