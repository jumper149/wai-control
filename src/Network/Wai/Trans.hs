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

import Control.Monad.IO.Unlift
import Network.Wai

-- | A type synonym for a wai 'Application' which has been lifted from the 'IO' monad.
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

-- | Lift a wai 'Application' to an 'ApplicationT'.
liftApplication :: MonadUnliftIO m
                => Application
                -> ApplicationT m
liftApplication app request respond = withRunInIO $ \ runInIO ->
  app request $ runInIO . respond

-- | Run an 'ApplicationT' in the inner monad.
runApplicationT :: MonadUnliftIO m
                => ApplicationT m
                -> m Application
runApplicationT appT = withRunInIO $ \ runInIO ->
  return $ \ request respond -> runInIO $ appT request $ liftIO . respond

-- | A type synonym for a wai 'Middleware' which has been lifted from the 'IO' monad.
type MiddlewareT m = ApplicationT m -> ApplicationT m

-- | Lift a wai 'Middleware' to a 'MiddlewareT'.
liftMiddleware :: MonadUnliftIO m
               => Middleware
               -> MiddlewareT m
liftMiddleware mid appT request respond = do
  app <- runApplicationT appT
  withRunInIO $ \ runInIO -> mid app request $ runInIO . respond

-- | Run a 'MiddlewareT' in the inner monad.
runMiddlewareT :: MonadUnliftIO m
               => MiddlewareT m
               -> m Middleware
runMiddlewareT midT = withRunInIO $ \ runInIO ->
  return $ \ app request respond -> do
    app' <- runInIO . runApplicationT . midT $ liftApplication app
    app' request respond
