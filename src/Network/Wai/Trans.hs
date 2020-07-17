{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Trans (

  -- * Application
  ApplicationT
, liftApplication
, runApplicationT

  -- * Middleware
, MiddlewareT
, liftMiddleware
, runMiddlewareT

) where

import Control.Monad.Base
import Control.Monad.Trans.Control.Identity
import Network.Wai

-- | A type synonym for a wai 'Application' which has been lifted from the 'IO' monad.
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

-- | Lift a wai 'Application' to an 'ApplicationT'.
liftApplication :: MonadBaseControlIdentity IO m
                => Application
                -> ApplicationT m
liftApplication app request respond = liftBaseWithIdentity $ \ runInBase ->
  app request $ runInBase . respond

-- | Run an 'ApplicationT' in the inner monad.
runApplicationT :: MonadBaseControlIdentity IO m
                => ApplicationT m
                -> m Application
runApplicationT appT = liftBaseWithIdentity $ \ runInBase ->
  return $ \ request respond -> runInBase $ appT request $ liftBase . respond

-- | A type synonym for a wai 'Middleware' which has been lifted from the 'IO' monad.
type MiddlewareT m = ApplicationT m -> ApplicationT m

-- | Lift a wai 'Middleware' to a 'MiddlewareT'.
liftMiddleware :: MonadBaseControlIdentity IO m
               => Middleware
               -> MiddlewareT m
liftMiddleware mid appT request respond = do
  app <- runApplicationT appT
  liftBaseWithIdentity $ \ runInBase -> mid app request $ runInBase . respond

-- | Run a 'MiddlewareT' in the inner monad.
runMiddlewareT :: MonadBaseControlIdentity IO m
               => MiddlewareT m
               -> m Middleware
runMiddlewareT midT = liftBaseWithIdentity $ \ runInBase ->
  return $ \ app request respond -> do
    app' <- runInBase . runApplicationT . midT $ liftApplication app
    app' request respond
