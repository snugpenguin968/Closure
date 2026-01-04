{-|
Module      : Logic.Normalizer
Description : Database schema normalization algorithms
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

The Scissors Layer - implements BCNF and 3NF decomposition algorithms.
Uses the Strategy pattern via Has-Pattern for dynamic algorithm selection.
-}
module Logic.Normalizer
  ( -- * Types
    Relation(..)
  , DecompositionResult(..)
  , DecompositionTree(..)
  , NormalizationHealth(..)
  , HealthLevel(..)
    -- * Key Finding
  , findCandidateKeys
  , isSuperkey
    -- * Normalization Checks
  , isBCNF
  , is3NF
  , checkHealth
    -- * Decomposition
  , normalize
  , normalizeWithTree
  , decomposeToBCNF
  , decomposeToBCNFWithTree
  , synthesizeTo3NF
  , optimizeForPerformance
    -- * FD Projection
  , projectFDs
  ) where

import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

import Logic.Attributes
import App.HasPattern

-- | A relation with attributes and functional dependencies
data Relation = Relation
  { relName  :: !Text
  , relAttrs :: !AttributeSet
  , relFDs   :: ![FD]
  } deriving stock (Eq, Show)

-- | Health level for a relation
data HealthLevel
  = Healthy       -- ^ In BCNF, no issues
  | Warning       -- ^ In 3NF but not BCNF
  | Critical      -- ^ Violates 3NF
  deriving stock (Eq, Ord, Show)

-- | Health status for a relation
data NormalizationHealth = NormalizationHealth
  { nhLevel       :: !HealthLevel
  , nhMessage     :: !Text
  , nhViolations  :: ![FD]           -- ^ FDs causing issues
  , nhSuggestion  :: !Text
  } deriving stock (Show)

-- | Tree structure showing decomposition history
data DecompositionTree
  = Leaf !Relation                                -- ^ Final relation (no more splits)
  | Split !Relation !FD !DecompositionTree !DecompositionTree  -- ^ Split on FD
  deriving stock (Show)

-- | Result of decomposition
data DecompositionResult = DecompositionResult
  { drRelations    :: ![Relation]               -- ^ The decomposed relations
  , drTree         :: !(Maybe DecompositionTree) -- ^ Optional decomposition tree
  , drForeignKeys  :: ![(Text, Text)]           -- ^ (FromTable, ToTable) FK relationships
  , drWarnings     :: ![Text]                   -- ^ Any warnings
  , drHealth       :: !(Maybe NormalizationHealth) -- ^ Pre-decomposition health 
  } deriving stock (Show)

-- | Check the normalization health of a relation
checkHealth :: Relation -> NormalizationHealth
checkHealth rel
  | isBCNF rel = NormalizationHealth
      { nhLevel = Healthy
      , nhMessage = "Relation is in BCNF - optimal integrity"
      , nhViolations = []
      , nhSuggestion = "No changes needed"
      }
  | is3NF rel = NormalizationHealth
      { nhLevel = Warning
      , nhMessage = "Relation is in 3NF but not BCNF"
      , nhViolations = findBCNFViolations rel
      , nhSuggestion = "Consider BCNF decomposition for maximum integrity"
      }
  | otherwise = NormalizationHealth
      { nhLevel = Critical
      , nhMessage = "Relation violates 3NF - update/delete anomalies possible"
      , nhViolations = find3NFViolations rel
      , nhSuggestion = "Normalize to at least 3NF to prevent data anomalies"
      }
  where
    findBCNFViolations r = filter (not . isBCNFCompliant r) (relFDs r)
    find3NFViolations r = filter (not . is3NFCompliant r) (relFDs r)
    
    isBCNFCompliant r (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | otherwise = isSuperkey lhs r
    
    is3NFCompliant r (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | isSuperkey lhs r = True
      | otherwise = all (`Set.member` primeAttrs) (Set.toList rhs)
      where
        primeAttrs = Set.unions (findCandidateKeys r)

-- | Check if a set of attributes is a superkey
isSuperkey :: AttributeSet -> Relation -> Bool
isSuperkey attrs (Relation _ allAttrs fds) =
  closure attrs fds == allAttrs

-- | Find all candidate keys of a relation
findCandidateKeys :: Relation -> [AttributeSet]
findCandidateKeys rel@(Relation _ attrs _) =
  filter (not . hasProperSubsetKey) superkeys
  where
    superkeys = filter (`isSuperkey` rel) $ powerSet attrs
    hasProperSubsetKey k = any (\k' -> k' `Set.isProperSubsetOf` k && k' `isSuperkey` rel) superkeys

-- | Power set of a set
powerSet :: Ord a => Set a -> [Set a]
powerSet s = map Set.fromList $ subsequences $ Set.toList s
  where
    subsequences [] = [[]]
    subsequences (x:xs) = let rest = subsequences xs in rest ++ map (x:) rest

-- | Check if a relation is in BCNF
isBCNF :: Relation -> Bool
isBCNF rel = all (isBCNFCompliant rel) (relFDs rel)
  where
    isBCNFCompliant r (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | otherwise = isSuperkey lhs r

-- | Check if a relation is in 3NF
is3NF :: Relation -> Bool
is3NF rel = all is3NFCompliant (relFDs rel)
  where
    candidateKeys = findCandidateKeys rel
    primeAttrs = Set.unions candidateKeys

    is3NFCompliant (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | isSuperkey lhs rel = True
      | otherwise = all (`Set.member` primeAttrs) (Set.toList rhs)

-- | Normalize a relation based on the strategy in the environment
normalize :: (HasOptimizationStrategy env, HasLog env) 
          => Relation -> AppM env DecompositionResult
normalize rel = do
  strategy <- asks getStrategy
  logInfo $ "Normalizing '" <> relName rel <> "' with strategy: " <> showStrategy strategy
  let health = checkHealth rel
  case strategy of
    PureIntegrity   -> pure $ addHealth health $ decomposeToBCNF rel
    Balanced        -> pure $ addHealth health $ synthesizeTo3NF rel
    HighPerformance -> do
      logWarn "High-performance mode: applying denormalization heuristics"
      pure $ addHealth health $ optimizeForPerformance rel
  where
    showStrategy PureIntegrity   = "BCNF (Pure Integrity)"
    showStrategy Balanced        = "3NF (Balanced)"
    showStrategy HighPerformance = "Denormalized (High Performance)"
    
    addHealth h result = result { drHealth = Just h }

-- | Normalize and return the decomposition tree
normalizeWithTree :: (HasOptimizationStrategy env, HasLog env)
                  => Relation -> AppM env DecompositionResult
normalizeWithTree rel = do
  strategy <- asks getStrategy
  logInfo $ "Normalizing with tree for '" <> relName rel <> "'"
  let health = checkHealth rel
  case strategy of
    PureIntegrity -> pure $ addHealth health $ decomposeToBCNFWithTree rel
    Balanced      -> pure $ addHealth health $ synthesizeTo3NF rel
    HighPerformance -> pure $ addHealth health $ optimizeForPerformance rel
  where
    addHealth h result = result { drHealth = Just h }

-- | Decompose to BCNF using the decomposition algorithm
decomposeToBCNF :: Relation -> DecompositionResult
decomposeToBCNF rel = 
  let withTree = decomposeToBCNFWithTree rel
  in withTree { drTree = Nothing }  -- Strip tree for lightweight result

-- | Decompose to BCNF and return the decomposition tree
decomposeToBCNFWithTree :: Relation -> DecompositionResult
decomposeToBCNFWithTree rel
  | isBCNF rel = DecompositionResult [rel] (Just (Leaf rel)) [] [] Nothing
  | otherwise = case findViolation rel of
      Nothing -> DecompositionResult [rel] (Just (Leaf rel)) [] [] Nothing
      Just fd@(FD lhs _) ->
        let -- R1 = closure(lhs)
            r1Attrs = closure lhs (relFDs rel)
            r1Name = relName rel <> "_" <> abbreviate lhs
            r1 = Relation r1Name r1Attrs (projectFDs (relFDs rel) r1Attrs)
            -- R2 = lhs ∪ (R - closure(lhs))
            r2Attrs = lhs `Set.union` (relAttrs rel `Set.difference` r1Attrs)
            r2Name = relName rel <> "_rest"
            r2 = Relation r2Name r2Attrs (projectFDs (relFDs rel) r2Attrs)
            -- Recursively decompose
            result1 = decomposeToBCNFWithTree r1
            result2 = decomposeToBCNFWithTree r2
            -- Build tree
            tree = Split rel fd 
                     (maybe (Leaf r1) id (drTree result1))
                     (maybe (Leaf r2) id (drTree result2))
        in DecompositionResult
            { drRelations = drRelations result1 ++ drRelations result2
            , drTree = Just tree
            , drForeignKeys = (r2Name, r1Name) : drForeignKeys result1 ++ drForeignKeys result2
            , drWarnings = ["BCNF decomposition may lose FD: " <> showFD fd] 
                           ++ drWarnings result1 ++ drWarnings result2
            , drHealth = Nothing
            }
  where
    findViolation r = 
      let violations = filter (not . isBCNFCompliant r) (relFDs r)
      in if null violations then Nothing else Just (head violations)
    
    isBCNFCompliant r (FD lhs rhs)
      | rhs `Set.isSubsetOf` lhs = True
      | otherwise = isSuperkey lhs r

    abbreviate attrs = T.take 10 $ T.intercalate "" $ Set.toList attrs
    showFD (FD l r) = T.intercalate "," (Set.toList l) <> " -> " <> T.intercalate "," (Set.toList r)

-- | Synthesize to 3NF using the synthesis algorithm
synthesizeTo3NF :: Relation -> DecompositionResult
synthesizeTo3NF rel@(Relation name _ fds) =
  let minimalFDs = getMinimalCover fds
      fdRelations = zipWith mkRelation [(1::Int)..] minimalFDs
      candidateKeys = findCandidateKeys rel
      hasKeyRelation = any (containsKey candidateKeys) fdRelations
      finalRelations = if hasKeyRelation
                        then fdRelations
                        else case candidateKeys of
                               [] -> fdRelations
                               (k:_) -> Relation (name <> "_key") k [] : fdRelations
      foreignKeys = inferForeignKeys finalRelations
  in DecompositionResult finalRelations Nothing foreignKeys [] Nothing
  where
    mkRelation :: Int -> FD -> Relation
    mkRelation i (FD lhs rhs) = 
      Relation (name <> "_" <> T.pack (show i)) (lhs `Set.union` rhs) [FD lhs rhs]
    
    containsKey keys (Relation _ relAttrs' _) = 
      any (`Set.isSubsetOf` relAttrs') keys
    
    -- Infer FK relationships from shared key attributes
    inferForeignKeys :: [Relation] -> [(Text, Text)]
    inferForeignKeys rels = 
      [ (relName r1, relName r2)
      | r1 <- rels
      , r2 <- rels
      , relName r1 /= relName r2
      , any (`Set.isSubsetOf` relAttrs r1) (findCandidateKeys r2)
      ]

-- | High-performance mode: keep snapshot attributes together
-- Uses heuristics to avoid costly joins for read-heavy patterns
optimizeForPerformance :: Relation -> DecompositionResult
optimizeForPerformance rel@(Relation name attrs fds)
  | null snapshotFDs = synthesizeTo3NF rel  -- No snapshot pattern, use 3NF
  | otherwise = 
      let -- Keep snapshot attributes in main table
          mainRel = Relation name attrs (filter (not . isSnapshotFD) fds)
          warnings = map formatSnapshotWarning snapshotFDs
      in DecompositionResult [mainRel] Nothing [] warnings Nothing
  where
    -- Heuristic: FDs with "At" suffix (e.g., PriceAtPurchase) are snapshots
    snapshotFDs = filter isSnapshotFD fds
    
    isSnapshotFD (FD _ rhs) = any isSnapshotAttr (Set.toList rhs)
    isSnapshotAttr attr = T.isInfixOf "At" attr || T.isInfixOf "Snapshot" attr
    
    formatSnapshotWarning (FD lhs rhs) = 
      "Kept denormalized for performance: " 
      <> T.intercalate "," (Set.toList lhs) <> " -> " 
      <> T.intercalate "," (Set.toList rhs)
      <> " (snapshot pattern detected)"

-- | Project FDs onto a subset of attributes
projectFDs :: [FD] -> AttributeSet -> [FD]
projectFDs fds targetAttrs = nub
  [ FD x (Set.singleton a)
  | x <- take 100 $ powerSet targetAttrs  -- Limit for large schemas
  , not (Set.null x)
  , a <- Set.toList targetAttrs
  , a `Set.notMember` x
  , a `Set.member` closure x fds
  ]
