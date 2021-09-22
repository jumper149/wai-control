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

-- * Websocket type Re-exports
, ServerApp
, ClientApp
, Connection
, ConnectionOptions
, PendingConnection
) where

import Control.Monad.Base ( MonadBase(..) )
import Control.Monad.Trans.Control.Identity
    ( MonadBaseControlIdentity(..) )
import Network.Wai.Handler.WebSockets ( websocketsOr )
import Network.WebSockets
    ( ConnectionOptions,
      ServerApp,
      PendingConnection,
      ClientApp,
      Connection )

import Network.Wai.Trans
    ( MiddlewareT, liftApplication, runApplicationT )

-- | A type synonym for a websockets 'ServerApp' which has been lifted from the 'IO' monad.
type ServerAppT m = PendingConnection -> m ()

-- | Lift a websockets 'ServerApp' to a 'ServerAppT'.
liftServerApp :: MonadBase IO m
              => ServerApp
              -> ServerAppT m
liftServerApp serverApp = liftBase . serverApp

-- | Run a 'ServerAppT' in the inner monad.
runServerAppT :: MonadBaseControlIdentity IO m
              => ServerAppT m
              -> m ServerApp
runServerAppT serverAppT = liftBaseWithIdentity $ \ runInBase ->
  return $ runInBase . serverAppT

-- | A type synonym for a websockets 'ClientApp' which has been lifted from the 'IO' monad.
type ClientAppT m a = Connection -> m a

-- | Lift a websockets 'ClientApp' to a 'ClientAppT'.
liftClientApp :: MonadBase IO m
              => ClientApp a
              -> ClientAppT m a
liftClientApp clientApp = liftBase . clientApp

-- | Run a 'ClientAppT' in the inner monad.
runClientAppT :: MonadBaseControlIdentity IO m
              => ClientAppT m a
              -> m (ClientApp a)
runClientAppT clientAppT = liftBaseWithIdentity $ \ runInBase ->
  return $ runInBase . clientAppT

{- | Upgrade a 'ServerAppT' to a 'MiddlewareT'.
  This function is based on 'websocketsOr'.
-}
websocketsOrT :: MonadBaseControlIdentity IO m
              => ConnectionOptions
              -> ServerAppT m
              -> MiddlewareT m
websocketsOrT options serverAppT appT request respond = do
  serverApp <- runServerAppT serverAppT
  app <- runApplicationT appT
  (liftApplication $ websocketsOr options serverApp app) request respond
