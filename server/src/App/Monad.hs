{-|
Module      : App.Monad
Description : Application monad with error handling
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

Defines the application monad stack with proper error handling using ExceptT.
-}
module App.Monad
  ( -- * Application Monad
    AppM
  , runAppM
  , runAppMEither
    -- * Error Handling
  , throwAppError
  , catchAppError
  , mapAppError
    -- * Lifting
  , liftIO
  , ask
  , asks
    -- * Re-exports
  , module App.Error
  , module App.HasPattern
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)

import App.Error
import App.HasPattern hiding (AppM, runAppM, logInfo, logDebug, logWarn)
import qualified App.HasPattern as HP

-- | The application monad with error handling
-- Stack: ExceptT AppError (ReaderT env IO) a
newtype AppM env a = AppM 
  { unAppM :: ExceptT AppError (ReaderT env IO) a 
  }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader env
    , MonadError AppError
    )

-- | Run the application monad, throwing on error
runAppM :: env -> AppM env a -> IO a
runAppM env action = do
  result <- runAppMEither env action
  case result of
    Left err -> error $ "AppM failed: " ++ show err
    Right a -> pure a

-- | Run the application monad, returning Either
runAppMEither :: env -> AppM env a -> IO (Either AppError a)
runAppMEither env action = 
  runReaderT (runExceptT (unAppM action)) env

-- | Throw an application error
throwAppError :: AppError -> AppM env a
throwAppError = throwError

-- | Catch and handle an application error
catchAppError :: AppM env a -> (AppError -> AppM env a) -> AppM env a
catchAppError = catchError

-- | Map over the error type
mapAppError :: (AppError -> AppError) -> AppM env a -> AppM env a
mapAppError f action = catchError action (throwError . f)

-- | Log at Info level within AppM
logInfo :: (HasLog env) => Text -> AppM env ()
logInfo msg = do
  env <- ask
  liftIO $ doLog env Info msg

-- | Log at Debug level within AppM
logDebug :: (HasLog env) => Text -> AppM env ()
logDebug msg = do
  env <- ask
  liftIO $ doLog env Debug msg

-- | Log at Warn level within AppM
logWarn :: (HasLog env) => Text -> AppM env ()
logWarn msg = do
  env <- ask
  liftIO $ doLog env Warn msg

-- | Log at Error level within AppM
logError :: (HasLog env) => Text -> AppM env ()
logError msg = do
  env <- ask
  liftIO $ doLog env HP.Error msg
