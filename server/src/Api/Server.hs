{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Api.Server
Description : REST API for the normalization engine
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

HTTP API layer using Servant for communication with the frontend.
Includes input validation and proper error handling.
-}
module Api.Server
  ( -- * API Type
    NormalizerAPI
  , api
    -- * Server
  , server
  , runServer
    -- * Request/Response Types
  , NormalizeRequest(..)
  , NormalizeResponse(..)
  , AnalyzeRequest(..)
  , AnalyzeResponse(..)
  , WorkspaceRequest(..)
  , WorkspaceResponse(..)
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant

import App.Error
import App.HasPattern
import Logic.Attributes
import Logic.Normalizer
import Logic.Workspace

--------------------------------------------------------------------------------
-- Request/Response Types
--------------------------------------------------------------------------------

-- | FD representation in JSON
data FDJSON = FDJSON
  { fjLhs :: ![Text]
  , fjRhs :: ![Text]
  } deriving stock (Generic, Show)

instance FromJSON FDJSON
instance ToJSON FDJSON

-- | Attribute representation in JSON (with type info)
data AttributeJSON = AttributeJSON
  { ajName :: !Text
  , ajType :: !Text
  } deriving stock (Generic, Show)

instance FromJSON AttributeJSON
instance ToJSON AttributeJSON

-- | Relation representation in JSON
data RelationJSON = RelationJSON
  { rjName       :: !Text
  , rjAttributes :: ![AttributeJSON]
  , rjFDs        :: ![FDJSON]
  } deriving stock (Generic, Show)

instance FromJSON RelationJSON
instance ToJSON RelationJSON

-- | Request to normalize a single relation
data NormalizeRequest = NormalizeRequest
  { nrRelation    :: !RelationJSON
  , nrStrategy    :: !Text
  , nrIncludeTree :: !Bool
  } deriving stock (Generic, Show)

instance FromJSON NormalizeRequest
instance ToJSON NormalizeRequest

-- | Health status in response
data HealthJSON = HealthJSON
  { hjLevel      :: !Text
  , hjMessage    :: !Text
  , hjViolations :: ![FDJSON]
  , hjSuggestion :: !Text
  } deriving stock (Generic, Show)

instance FromJSON HealthJSON
instance ToJSON HealthJSON

-- | Tree node in response
data TreeNodeJSON = TreeNodeJSON
  { tnRelation :: !RelationJSON
  , tnSplitFD  :: !(Maybe FDJSON)
  , tnChildren :: ![TreeNodeJSON]
  } deriving stock (Generic, Show)

instance FromJSON TreeNodeJSON
instance ToJSON TreeNodeJSON

-- | Response from normalization
data NormalizeResponse = NormalizeResponse
  { nresSuccess     :: !Bool
  , nresRelations   :: ![RelationJSON]
  , nresTree        :: !(Maybe TreeNodeJSON)
  , nresForeignKeys :: ![(Text, Text)]
  , nresWarnings    :: ![Text]
  , nresHealth      :: !(Maybe HealthJSON)
  , nresError       :: !(Maybe Text)
  } deriving stock (Generic, Show)

instance FromJSON NormalizeResponse
instance ToJSON NormalizeResponse

-- | Request to analyze without decomposing
data AnalyzeRequest = AnalyzeRequest
  { arRelation :: !RelationJSON
  } deriving stock (Generic, Show)

instance FromJSON AnalyzeRequest
instance ToJSON AnalyzeRequest

-- | Response from analysis
data AnalyzeResponse = AnalyzeResponse
  { aresSuccess       :: !Bool
  , aresHealth        :: !(Maybe HealthJSON)
  , aresCandidateKeys :: ![[Text]]
  , aresIsBCNF        :: !Bool
  , aresIs3NF         :: !Bool
  , aresError         :: !(Maybe Text)
  } deriving stock (Generic, Show)

instance FromJSON AnalyzeResponse
instance ToJSON AnalyzeResponse

-- | Request for workspace operations
data WorkspaceRequest = WorkspaceRequest
  { wrRelations :: ![RelationJSON]
  , wrStrategy  :: !Text
  } deriving stock (Generic, Show)

instance FromJSON WorkspaceRequest
instance ToJSON WorkspaceRequest

-- | Cross-table FD in response
data CrossTableFDJSON = CrossTableFDJSON
  { ctjFromTable  :: !Text
  , ctjToTable    :: !Text
  , ctjFD         :: !FDJSON
  , ctjSuggestion :: !Text
  } deriving stock (Generic, Show)

instance FromJSON CrossTableFDJSON
instance ToJSON CrossTableFDJSON

-- | Table health in response
data TableHealthJSON = TableHealthJSON
  { thjTableName  :: !Text
  , thjSeverity   :: !Text
  , thjMessage    :: !Text
  , thjSuggestion :: !Text
  } deriving stock (Generic, Show)

instance FromJSON TableHealthJSON
instance ToJSON TableHealthJSON

-- | Response from workspace operations
data WorkspaceResponse = WorkspaceResponse
  { wresSuccess          :: !Bool
  , wresResults          :: ![(Text, [RelationJSON])]
  , wresCrossTableFDs    :: ![CrossTableFDJSON]
  , wresHealth           :: ![TableHealthJSON]
  , wresMergeSuggestions :: ![(Text, Text, Text)]
  , wresError            :: !(Maybe Text)
  } deriving stock (Generic, Show)

instance FromJSON WorkspaceResponse
instance ToJSON WorkspaceResponse

--------------------------------------------------------------------------------
-- Input Validation
--------------------------------------------------------------------------------

-- | Validate a relation JSON input
validateRelation :: RelationJSON -> Either AppError Relation
validateRelation (RelationJSON name attrs fds) = do
  when (T.null (T.strip name)) $
    Left $ validationError $ EmptyField "relationName"
  
  when (null attrs) $
    Left $ validationError $ EmptyField "attributes"
  
  let attrNames = map (T.strip . ajName) attrs
      emptyAttrs = filter T.null attrNames
  when (not (null emptyAttrs)) $
    Left $ validationError $ InvalidValue "attributes" "contains empty attribute names"
  
  let allAttrs = Set.fromList attrNames
  mapM_ (validateFD allAttrs) fds
  
  let validFDs = map jsonToFD fds
  Right $ Relation (T.strip name) allAttrs validFDs

-- | Validate an FD references valid attributes
validateFD :: Set.Set Text -> FDJSON -> Either AppError ()
validateFD validAttrs (FDJSON lhs rhs) = do
  when (null lhs) $
    Left $ validationError $ InvalidValue "fd.lhs" "left-hand side cannot be empty"
  when (null rhs) $
    Left $ validationError $ InvalidValue "fd.rhs" "right-hand side cannot be empty"
  
  let lhsSet = Set.fromList (map T.strip lhs)
      rhsSet = Set.fromList (map T.strip rhs)
      invalidLhs = lhsSet `Set.difference` validAttrs
      invalidRhs = rhsSet `Set.difference` validAttrs
  
  when (not (Set.null invalidLhs)) $
    Left $ validationError $ InvalidValue "fd.lhs" $ 
      "unknown attributes: " <> T.intercalate ", " (Set.toList invalidLhs)
  
  when (not (Set.null invalidRhs)) $
    Left $ validationError $ InvalidValue "fd.rhs" $ 
      "unknown attributes: " <> T.intercalate ", " (Set.toList invalidRhs)

-- | Validate strategy value
validateStrategy :: Text -> Either AppError OptimizationStrategy
validateStrategy s
  | s == "bcnf" = Right PureIntegrity
  | s == "3nf" = Right Balanced
  | s == "performance" = Right HighPerformance
  | T.null (T.strip s) = Right Balanced  -- Default
  | otherwise = Left $ validationError $ InvalidValue "strategy" $
      "must be 'bcnf', '3nf', or 'performance', got: " <> s

--------------------------------------------------------------------------------
-- Converters
--------------------------------------------------------------------------------

jsonToFD :: FDJSON -> FD
jsonToFD (FDJSON lhs rhs) = FD (Set.fromList (map T.strip lhs)) (Set.fromList (map T.strip rhs))

fdToJSON :: FD -> FDJSON
fdToJSON (FD lhs rhs) = FDJSON (Set.toList lhs) (Set.toList rhs)

relationToJSON :: Relation -> RelationJSON
relationToJSON (Relation name attrs fds) =
  RelationJSON name (map (\a -> AttributeJSON a "TEXT") (Set.toList attrs)) (map fdToJSON fds)

healthToJSON :: NormalizationHealth -> HealthJSON
healthToJSON nh = HealthJSON
  { hjLevel = levelToText (nhLevel nh)
  , hjMessage = nhMessage nh
  , hjViolations = map fdToJSON (nhViolations nh)
  , hjSuggestion = nhSuggestion nh
  }
  where
    levelToText Healthy  = "healthy"
    levelToText Warning  = "warning"
    levelToText Critical = "critical"

treeToJSON :: DecompositionTree -> TreeNodeJSON
treeToJSON (Leaf rel) = TreeNodeJSON (relationToJSON rel) Nothing []
treeToJSON (Split rel fd left right) = TreeNodeJSON
  { tnRelation = relationToJSON rel
  , tnSplitFD = Just (fdToJSON fd)
  , tnChildren = [treeToJSON left, treeToJSON right]
  }

--------------------------------------------------------------------------------
-- API Type
--------------------------------------------------------------------------------

type NormalizerAPI = 
       "api" :> "normalize" :> ReqBody '[JSON] NormalizeRequest :> Post '[JSON] NormalizeResponse
  :<|> "api" :> "analyze" :> ReqBody '[JSON] AnalyzeRequest :> Post '[JSON] AnalyzeResponse
  :<|> "api" :> "workspace" :> ReqBody '[JSON] WorkspaceRequest :> Post '[JSON] WorkspaceResponse
  :<|> "api" :> "health" :> Get '[JSON] Text

api :: Proxy NormalizerAPI
api = Proxy

--------------------------------------------------------------------------------
-- Server Implementation
--------------------------------------------------------------------------------

server :: Server NormalizerAPI
server = normalizeHandler :<|> analyzeHandler :<|> workspaceHandler :<|> healthHandler
  where
    healthHandler :: Handler Text
    healthHandler = pure "OK"

    normalizeHandler :: NormalizeRequest -> Handler NormalizeResponse
    normalizeHandler req = do
      -- Validate inputs
      case (,) <$> validateRelation (nrRelation req) <*> validateStrategy (nrStrategy req) of
        Left err -> pure $ errorResponse err
        Right (rel, strategy) -> do
          let env = AppEnv strategy Info
          result <- liftIO $ runAppM env $
            if nrIncludeTree req
              then normalizeWithTree rel
              else normalize rel
          
          pure NormalizeResponse
            { nresSuccess = True
            , nresRelations = map relationToJSON (drRelations result)
            , nresTree = treeToJSON <$> drTree result
            , nresForeignKeys = drForeignKeys result
            , nresWarnings = drWarnings result
            , nresHealth = healthToJSON <$> drHealth result
            , nresError = Nothing
            }
      where
        errorResponse err = NormalizeResponse
          { nresSuccess = False
          , nresRelations = []
          , nresTree = Nothing
          , nresForeignKeys = []
          , nresWarnings = []
          , nresHealth = Nothing
          , nresError = Just (errorToText err)
          }

    analyzeHandler :: AnalyzeRequest -> Handler AnalyzeResponse
    analyzeHandler req = do
      case validateRelation (arRelation req) of
        Left err -> pure $ errorResponse err
        Right rel -> do
          let health = checkHealth rel
              candidateKeys = findCandidateKeys rel
          
          pure AnalyzeResponse
            { aresSuccess = True
            , aresHealth = Just (healthToJSON health)
            , aresCandidateKeys = map Set.toList candidateKeys
            , aresIsBCNF = isBCNF rel
            , aresIs3NF = is3NF rel
            , aresError = Nothing
            }
      where
        errorResponse err = AnalyzeResponse
          { aresSuccess = False
          , aresHealth = Nothing
          , aresCandidateKeys = []
          , aresIsBCNF = False
          , aresIs3NF = False
          , aresError = Just (errorToText err)
          }

    workspaceHandler :: WorkspaceRequest -> Handler WorkspaceResponse
    workspaceHandler req = do
      -- Validate all relations
      let validations = map validateRelation (wrRelations req)
          (errors, rels) = partitionResults validations
      
      case (errors, validateStrategy (wrStrategy req)) of
        (e:_, _) -> pure $ errorResponse e
        (_, Left err) -> pure $ errorResponse err
        ([], Right strategy) -> do
          let workspace = foldr addRelation emptyWorkspace rels
              env = AppEnv strategy Info
          
          results <- liftIO $ runAppM env $ optimizeWorkspace workspace
          let crossFDs = wsCrossTableFDs workspace
              health = analyzeHealth workspace
              merges = suggestMerges workspace
          
          pure WorkspaceResponse
            { wresSuccess = True
            , wresResults = [(name, map relationToJSON (drRelations dr)) | (name, dr) <- results]
            , wresCrossTableFDs = map crossFDToJSON crossFDs
            , wresHealth = map tableHealthToJSON health
            , wresMergeSuggestions = merges
            , wresError = Nothing
            }
      where
        errorResponse err = WorkspaceResponse
          { wresSuccess = False
          , wresResults = []
          , wresCrossTableFDs = []
          , wresHealth = []
          , wresMergeSuggestions = []
          , wresError = Just (errorToText err)
          }
        
        partitionResults = foldr go ([], [])
          where
            go (Left e) (es, rs) = (e:es, rs)
            go (Right r) (es, rs) = (es, r:rs)

    crossFDToJSON :: CrossTableFD -> CrossTableFDJSON
    crossFDToJSON ctfd = CrossTableFDJSON
      { ctjFromTable = ctFromTable ctfd
      , ctjToTable = ctToTable ctfd
      , ctjFD = fdToJSON (ctFD ctfd)
      , ctjSuggestion = ctSuggestion ctfd
      }
    
    tableHealthToJSON :: TableHealth -> TableHealthJSON
    tableHealthToJSON th = TableHealthJSON
      { thjTableName = thTableName th
      , thjSeverity = severityToText (thSeverity th)
      , thjMessage = thMessage th
      , thjSuggestion = thSuggestion th
      }
    
    severityToText HealthOK = "ok"
    severityToText HealthWarning = "warning"
    severityToText HealthError = "error"

--------------------------------------------------------------------------------
-- CORS and Server Startup
--------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy = cors $ const $ Just CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Just 3600
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Starting Closure server on port " ++ show port
  putStrLn "Endpoints:"
  putStrLn "  POST /api/normalize  - Normalize a relation"
  putStrLn "  POST /api/analyze    - Analyze health without decomposition"
  putStrLn "  POST /api/workspace  - Multi-table optimization"
  putStrLn "  GET  /api/health     - Health check"
  run port $ corsPolicy $ serve api server
