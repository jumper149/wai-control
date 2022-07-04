{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Handler.WebSockets.Trans (

-- * ServerApp
  ServerAppT
, liftServerApp
, runServerAppT

-- * ClientApp
, ClientAppT
, liftClientApp
, runClientAppT

-- * WebSocket
, websocketsOrT

) where

import Control.Monad.IO.Unlift
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Network.Wai.Trans

-- | A type synonym for a websockets 'ServerApp' which has been lifted from the 'IO' monad.
type ServerAppT m = PendingConnection -> m ()

-- | Lift a websockets 'ServerApp' to a 'ServerAppT'.
liftServerApp :: MonadIO m
              => ServerApp
              -> ServerAppT m
liftServerApp serverApp = liftIO . serverApp

-- | Run a 'ServerAppT' in the inner monad.
runServerAppT :: MonadUnliftIO m
              => ServerAppT m
              -> m ServerApp
runServerAppT serverAppT = withRunInIO $ \ runInIO ->
  return $ runInIO . serverAppT

-- | A type synonym for a websockets 'ClientApp' which has been lifted from the 'IO' monad.
type ClientAppT m a = Connection -> m a

-- | Lift a websockets 'ClientApp' to a 'ClientAppT'.
liftClientApp :: MonadIO m
              => ClientApp a
              -> ClientAppT m a
liftClientApp clientApp = liftIO . clientApp

-- | Run a 'ClientAppT' in the inner monad.
runClientAppT :: MonadUnliftIO m
              => ClientAppT m a
              -> m (ClientApp a)
runClientAppT clientAppT = withRunInIO $ \ runInIO ->
  return $ runInIO . clientAppT

{- | Upgrade a 'ServerAppT' to a 'MiddlewareT'.
  This function is based on 'websocketsOr'.
-}
websocketsOrT :: MonadUnliftIO m
              => ConnectionOptions
              -> ServerAppT m
              -> MiddlewareT m
websocketsOrT options serverAppT appT request respond = do
  serverApp <- runServerAppT serverAppT
  app <- runApplicationT appT
  (liftApplication $ websocketsOr options serverApp app) request respond
