{-|
Module      : App.HasPattern
Description : Has-Pattern for dependency injection
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

The Environment Layer - uses ReaderT Has-Pattern for dependency injection.
Allows swapping decomposition strategies (BCNF vs 3NF) dynamically.
-}
module App.HasPattern
  ( -- * Optimization Strategy
    OptimizationStrategy(..)
  , HasOptimizationStrategy(..)
    -- * Logging
  , LogLevel(..)
  , HasLog(..)
  , logInfo
  , logDebug
  , logWarn
    -- * Application Environment
  , AppEnv(..)
  , defaultEnv
    -- * Application Monad
  , AppM
  , runAppM
  ) where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- | Optimization strategy for decomposition
data OptimizationStrategy
  = PureIntegrity    -- ^ BCNF: ruthless decomposition for maximum integrity
  | Balanced         -- ^ 3NF: preserves FDs at cost of minor redundancy
  | HighPerformance  -- ^ Denormalized: keep some redundancy for read speed
  deriving stock (Eq, Show)

-- | Typeclass for environments that provide an optimization strategy
class HasOptimizationStrategy env where
  getStrategy :: env -> OptimizationStrategy

-- | Log levels
data LogLevel = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show)

-- | Typeclass for environments that support logging
class HasLog env where
  getLogLevel :: env -> LogLevel
  doLog :: env -> LogLevel -> Text -> IO ()

-- | The application environment
data AppEnv = AppEnv
  { envStrategy :: !OptimizationStrategy
  , envLogLevel :: !LogLevel
  }

-- | Default environment
defaultEnv :: AppEnv
defaultEnv = AppEnv
  { envStrategy = Balanced
  , envLogLevel = Info
  }

instance HasOptimizationStrategy AppEnv where
  getStrategy = envStrategy

instance HasLog AppEnv where
  getLogLevel = envLogLevel
  doLog env level msg
    | level >= getLogLevel env = TIO.hPutStrLn stderr $ "[" <> showLevel level <> "] " <> msg
    | otherwise = pure ()
    where
      showLevel Debug = "DEBUG"
      showLevel Info  = "INFO"
      showLevel Warn  = "WARN"
      showLevel Error = "ERROR"

-- | The application monad
type AppM env a = ReaderT env IO a

-- | Run the application monad
runAppM :: env -> AppM env a -> IO a
runAppM = flip runReaderT

-- | Log at Info level
logInfo :: (HasLog env) => Text -> AppM env ()
logInfo msg = do
  env <- ask
  liftIO $ doLog env Info msg

-- | Log at Debug level
logDebug :: (HasLog env) => Text -> AppM env ()
logDebug msg = do
  env <- ask
  liftIO $ doLog env Debug msg

-- | Log at Warn level
logWarn :: (HasLog env) => Text -> AppM env ()
logWarn msg = do
  env <- ask
  liftIO $ doLog env Warn msg
