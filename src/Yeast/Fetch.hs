{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yeast.Fetch
  ( fetch
  , fetchMany
  , fetchManyWith
  , FetchError(..)
  )
  where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative           ((<$>))
#endif
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

data FetchError
  = TimeoutFailure
  | DownloadFailure HttpException
  | UnknownFailure
  | ParseFailure ParseError
  deriving Show

fetch
  :: String                       -- ^ URL.
  -> IO (Either FetchError Feed)
fetch url = head <$> fetchMany [url]

fetchMany
  :: [String]                     -- ^ URLs.
  -> IO [Either FetchError Feed]
fetchMany = fetchManyWith opts (const (return ()))
  where
  opts = defaults & redirects .~ 3
                  & manager   .~ Left tlsManagerSettings

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
        , Handler (\(_ :: SomeException) ->
            return $ Left UnknownFailure)
        ]
    callback ef
    return ef
    where
    maybeM :: Monad m => b -> (a -> b) -> m (Maybe a) -> m b
    maybeM n j mmx = mmx >>= maybe (return n) (return . j)
