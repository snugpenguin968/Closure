{-|
Module      : Logic.Workspace
Description : Multi-table workspace coordination
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

Global workspace model for coordinating multiple tables. Handles cross-table
FD discovery and global optimization suggestions.
-}
module Logic.Workspace
  ( -- * Types
    Workspace(..)
  , CrossTableFD(..)
  , TableHealth(..)
  , HealthSeverity(..)
    -- * Workspace Operations
  , emptyWorkspace
  , addRelation
  , removeRelation
  , getRelation
    -- * Cross-Table Analysis
  , discoverCrossTableFDs
  , suggestMerges
  , analyzeHealth
    -- * Global Optimization
  , optimizeWorkspace
  ) where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Logic.Attributes
import Logic.Normalizer
import App.HasPattern

-- | A workspace containing multiple relations
data Workspace = Workspace
  { wsRelations    :: !(Map Text Relation)       -- ^ Tables by name
  , wsCrossTableFDs :: ![CrossTableFD]           -- ^ FDs spanning tables
  } deriving stock (Show)

-- | A functional dependency that spans two tables
data CrossTableFD = CrossTableFD
  { ctFromTable  :: !Text          -- ^ Source table name
  , ctToTable    :: !Text          -- ^ Target table name
  , ctFD         :: !FD            -- ^ The functional dependency
  , ctSuggestion :: !Text          -- ^ Optimization suggestion
  } deriving stock (Show, Eq)

-- | Health severity levels for table warnings
data HealthSeverity
  = HealthOK       -- ^ No issues
  | HealthWarning  -- ^ Minor issue (yellow)
  | HealthError    -- ^ Major issue (red)
  deriving stock (Eq, Ord, Show)

-- | Health status for a single table
data TableHealth = TableHealth
  { thTableName    :: !Text
  , thSeverity     :: !HealthSeverity
  , thMessage      :: !Text
  , thViolatingFDs :: ![FD]          -- ^ FDs causing the issue
  , thSuggestion   :: !Text          -- ^ Recommended action
  } deriving stock (Show)

-- | Create an empty workspace
emptyWorkspace :: Workspace
emptyWorkspace = Workspace Map.empty []

-- | Add a relation to the workspace
addRelation :: Relation -> Workspace -> Workspace
addRelation rel ws = ws
  { wsRelations = Map.insert (relName rel) rel (wsRelations ws)
  , wsCrossTableFDs = discoverCrossTableFDs (Map.insert (relName rel) rel (wsRelations ws))
  }

-- | Remove a relation from the workspace
removeRelation :: Text -> Workspace -> Workspace
removeRelation name ws = ws
  { wsRelations = Map.delete name (wsRelations ws)
  , wsCrossTableFDs = filter notInvolvingTable (wsCrossTableFDs ws)
  }
  where
    notInvolvingTable ctfd = ctFromTable ctfd /= name && ctToTable ctfd /= name

-- | Get a relation by name
getRelation :: Text -> Workspace -> Maybe Relation
getRelation name ws = Map.lookup name (wsRelations ws)

-- | Discover FDs that span multiple tables
-- If an attribute in Table A is determined by attributes in Table B,
-- that's a cross-table FD
discoverCrossTableFDs :: Map Text Relation -> [CrossTableFD]
discoverCrossTableFDs rels = 
  [ CrossTableFD
      { ctFromTable = nameA
      , ctToTable = nameB
      , ctFD = FD lhs (Set.singleton attr)
      , ctSuggestion = suggestionFor nameA nameB attr
      }
  | (nameA, relA) <- Map.toList rels
  , (nameB, relB) <- Map.toList rels
  , nameA /= nameB
  , attr <- Set.toList (relAttrs relA)
  , lhs <- potentialLHS relB
  , attr `Set.member` closure lhs (relFDs relB)
  ]
  where
    -- Get potential LHS sets from a relation (superkeys and key subsets)
    potentialLHS rel = take 10 $ -- Limit to avoid exponential blowup
      filter (not . Set.null) $ 
      map Set.fromList $ 
      subsequences $ 
      Set.toList (relAttrs rel)
    
    subsequences [] = [[]]
    subsequences (x:xs) = let rest = subsequences xs in rest ++ map (x:) rest
    
    suggestionFor fromT toT attr = 
      "Attribute '" <> attr <> "' in '" <> fromT <> 
      "' may be determined by '" <> toT <> "'. Consider FK relationship."

-- | Suggest table merges based on cross-table FDs
suggestMerges :: Workspace -> [(Text, Text, Text)]  -- ^ (TableA, TableB, Reason)
suggestMerges ws = 
  [ (ctFromTable ctfd, ctToTable ctfd, 
     "Tables share determining attributes via: " <> showFD (ctFD ctfd))
  | ctfd <- wsCrossTableFDs ws
  , shouldMerge ctfd
  ]
  where
    shouldMerge ctfd = 
      -- If the FD covers most of the source table, suggest merge
      let sourceRel = Map.lookup (ctFromTable ctfd) (wsRelations ws)
      in case sourceRel of
           Nothing -> False
           Just rel -> 
             let fdCovers = Set.size (fdRhs (ctFD ctfd)) * 2 >= Set.size (relAttrs rel)
             in fdCovers
    
    showFD (FD l r) = T.intercalate "," (Set.toList l) <> " -> " <> T.intercalate "," (Set.toList r)

-- | Analyze the health of all tables in the workspace
analyzeHealth :: Workspace -> [TableHealth]
analyzeHealth ws = map analyzeTable (Map.elems (wsRelations ws))
  where
    analyzeTable rel
      | isBCNF rel = TableHealth
          { thTableName = relName rel
          , thSeverity = HealthOK
          , thMessage = "Table is in BCNF"
          , thViolatingFDs = []
          , thSuggestion = "No action needed"
          }
      | is3NF rel = TableHealth
          { thTableName = relName rel
          , thSeverity = HealthWarning
          , thMessage = "Table is in 3NF but not BCNF"
          , thViolatingFDs = findBCNFViolations rel
          , thSuggestion = "Consider BCNF decomposition for maximum integrity"
          }
      | otherwise = TableHealth
          { thTableName = relName rel
          , thSeverity = HealthError
          , thMessage = "Table violates 3NF"
          , thViolatingFDs = find3NFViolations rel
          , thSuggestion = "Normalization required to prevent anomalies"
          }
    
    findBCNFViolations rel = filter (not . isBCNFCompliant rel) (relFDs rel)
    find3NFViolations rel = filter (not . is3NFCompliant rel) (relFDs rel)
    
    isBCNFCompliant rel (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | otherwise = isSuperkey lhs rel
    
    is3NFCompliant rel (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | isSuperkey lhs rel = True
      | otherwise = all (`Set.member` primeAttrs) (Set.toList rhs)
      where
        primeAttrs = Set.unions (findCandidateKeys rel)

-- | Optimize the entire workspace based on strategy
optimizeWorkspace :: (HasOptimizationStrategy env, HasLog env) 
                  => Workspace -> AppM env [(Text, DecompositionResult)]
optimizeWorkspace ws = do
  results <- mapM normalizeWithName (Map.toList (wsRelations ws))
  logInfo $ "Optimized " <> T.pack (show (length results)) <> " tables"
  pure results
  where
    normalizeWithName (name, rel) = do
      result <- normalize rel
      pure (name, result)
