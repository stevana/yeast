{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK show-extensions #-}

------------------------------------------------------------------------
-- |
-- Module      :  Yeast.Fetch
-- Copyright   :  (c) 2015-2016 Stevan Andjelkovic
-- License     :  ISC (see the file LICENSE)
-- Maintainer  :  Stevan Andjelkovic
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains functions for fetching news feeds from URLs.
--
------------------------------------------------------------------------

module Yeast.Fetch (
  -- * Fetching
  -- $fetching
    fetch
  , fetchMany
  , fetchManyWith

  -- * Fetching errors
  -- $errors
  , FetchError(..)
  )
  where

import           Control.Concurrent.ParallelIO (parallel)
import           Control.Exception             (Handler(..), SomeException,
                                                catches)
import           Control.Lens                  ((.~), (&), view, bimap)
import           Network.HTTP.Client           (HttpException)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Network.Wreq                  (Options, getWith,
                                                responseBody, manager,
                                                redirects, defaults)
import           System.Timeout                (timeout)

import           Yeast.Feed
import           Yeast.Parse

------------------------------------------------------------------------
-- $fetching
-- We can fetch feeds in various ways.

-- | Fetch a single feed.
fetch
  :: String                       -- ^ URL.
  -> IO (Either FetchError Feed)
fetch url = head <$> fetchMany [url]

-- | Fetch many feeds in parallel.
fetchMany
  :: [String]                     -- ^ URLs.
  -> IO [Either FetchError Feed]
fetchMany = fetchManyWith opts (const (return ()))
  where
  opts = defaults & redirects .~ 3
                  & manager   .~ Left tlsManagerSettings

-- | Fetch many feeds in parallel using a provided client configuration
-- and callback.
fetchManyWith
  :: Options                           -- ^ Client configuration.
  -> (Either FetchError Feed -> IO ()) -- ^ Callback (might be useful
                                       --   for reporting progress).
  -> [String]                          -- ^ URLs.
  -> IO [Either FetchError Feed]
fetchManyWith opts callback = parallel . map helper
  where
  helper :: String -> IO (Either FetchError Feed)
  helper url = do
    ef <- maybeM (Left TimeoutFailure)
                 (bimap ParseFailure id .
                    parseLBS . view responseBody)
                 (timeout (3 * 1000000) (getWith opts url))
      `catches`
        [ Handler (\(e :: HttpException) ->
            return $ Left $ DownloadFailure e)
        , Handler (\(e :: SomeException) ->
            return $ Left $ UnknownFailure e)
        ]
    callback ef
    return ef
    where
    maybeM :: Monad m => b -> (a -> b) -> m (Maybe a) -> m b
    maybeM n j mmx = mmx >>= maybe (return n) (return . j)

------------------------------------------------------------------------
-- $errors
-- Sometimes errors can occur while fetching feeds.

-- | Datatype which captures all ways a fetch can fail.
data FetchError
  = TimeoutFailure
      -- ^ Fetching took longer than (the hardcoded) three seconds.
  | DownloadFailure HttpException
      -- ^ The 'getWith' function from "Network.Wreq", which uses
      -- "Network.HTTP.Client" threw an error.
  | ParseFailure ParseError
      -- ^ The 'parseLBS' function from "Yeast.Parse" threw an error.
  | UnknownFailure SomeException
      -- ^ Some other exception was thrown.
  deriving Show
