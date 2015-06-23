{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Yeast.Serve
  ( API
  , ServeM
  , server
  , api
  , transform
  , runServer
  , withServer
  , withServedFeed
  , withFileFeed
  , baseUrl
  , addFeed
  , removeFeed
  , listFeed
  , viewFeed
  )
  where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative        ((<$>))
#endif
import           Control.Concurrent         (forkIO, killThread,
                                             threadDelay)
import           Control.Exception          (bracket)
import           Control.Lens               ((^.), (%~), (&), mapped)
import           Control.Monad.State        (State, runState, get, modify,
                                             liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Hashable              (hash)
import           Data.IORef                 (IORef, newIORef, readIORef,
                                             writeIORef)
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as M
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           Network.Wai.Handler.Warp   (run)
import           Servant                    (ServerT, (:<|>)((:<|>)), Get,
                                             Capture, PlainText, JSON, (:>),
                                             Proxy(Proxy), ServantErr, (:~>)
                                             (Nat), serve, enter)
import           Servant.Client             (ServantError, BaseUrl(BaseUrl),
                                             Scheme(Http), client,
                                             showBaseUrl)

import           Yeast.Feed
import           Yeast.Fetch
import           Yeast.Parse
import           Yeast.Render

------------------------------------------------------------------------
-- * API

type API
  =    "feed" :> "add"    :> Capture "feed" Feed :> Get '[JSON] Int
  :<|> "feed" :> "remove" :> Capture "hash" Int  :> Get '[JSON] ()
  :<|> "feed" :> "list"   :> Get '[PlainText] Text
  :<|> "feed" :> "view"   :> Capture "hash" Int  :> Get '[PlainText] Text

------------------------------------------------------------------------
-- * Server part

type ServeM = State (IntMap Feed)

server :: ServerT API ServeM
server
  =    feedAdd
  :<|> feedRemove
  :<|> feedList
  :<|> feedView
  where
  feedAdd :: Feed -> ServeM Int
  feedAdd f = do
    let i = hash (f^.title) -- XXX: `hashWithSalt` ?
    modify $ M.insert i f
    return i

  feedRemove :: Int -> ServeM ()
  feedRemove i = modify $ M.delete i

  feedList :: ServeM Text
  feedList
    = T.pack
    . show
    . map (\(i, f) -> (f^.title & mapped %~ T.fromStrict, i))
    . M.toList <$> get

  feedView :: Int -> ServeM Text
  feedView i
    = maybe "Feed not found." (render (def { rsPretty = True }))
    . M.lookup i <$> get

api :: Proxy API
api = Proxy

transform :: IORef (IntMap Feed) -> ServeM :~> EitherT ServantErr IO
transform ref = Nat $ \m -> liftIO $ do
  s <- readIORef ref
  let (x, s') = runState m s
  writeIORef ref s'
  return x

runServer
  :: [Feed] -- ^ Initial feeds.
  -> IO ()
runServer fs = do
  let m = foldr (\f ih -> M.insert (hash (f^.title)) f ih) M.empty fs
  ref <- newIORef m
  run 8081 $ serve api $ enter (transform ref) server

withServer :: [Feed] -> IO a -> IO a
withServer fs io = bracket (forkIO $ runServer fs) killThread $ const $ do

  -- Wait a bit for the server to start running.
  threadDelay 500000
  io

withServedFeed
  :: Feed                  -- ^ Feed to serve.
  -> (String -> IO ())     -- ^ Failure continuation.
  -> (Feed -> IO ())       -- ^ Success continuation.
  -> IO ()
withServedFeed f e k = withServer [] $ do
  ei <- runEitherT $ addFeed f
  case ei of
    Left err -> e $ show err
    Right i  -> do
      ef <- fetch $ showBaseUrl baseUrl ++ "/feed/view/" ++ show i
      case ef of
        Left err -> e $ show err
        Right f' -> k f'

withFileFeed
  :: FilePath
  -> (String -> IO ())
  -> (Feed -> IO ())
  -> IO ()
withFileFeed fp e k = do
  ef <- parseFile fp
  case ef of
    Left err -> e $ show err
    Right f  -> k f

------------------------------------------------------------------------
-- * Client part

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081

addFeed    :: Feed -> EitherT ServantError IO Int
removeFeed :: Int  -> EitherT ServantError IO ()
listFeed   ::         EitherT ServantError IO Text
viewFeed   :: Int  -> EitherT ServantError IO Text
addFeed :<|> removeFeed :<|> listFeed :<|> viewFeed = client api baseUrl
