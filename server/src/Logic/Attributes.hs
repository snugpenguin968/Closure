{-|
Module      : Logic.Attributes
Description : Attribute closure and minimal cover calculations
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

The Atom Layer - calculates closures and minimal covers for functional dependencies.
-}
module Logic.Attributes
  ( -- * Types
    Attribute
  , AttributeSet
  , FD(..)
    -- * Closure Operations
  , closure
  , closureUnderFDs
    -- * Minimal Cover
  , getMinimalCover
  , removeExtraneous
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List (foldl')

-- | An attribute is represented as text
type Attribute = Text

-- | A set of attributes
type AttributeSet = Set Attribute

-- | A Functional Dependency: lhs -> rhs
data FD = FD
  { fdLhs :: !AttributeSet  -- ^ Left-hand side (determinant)
  , fdRhs :: !AttributeSet  -- ^ Right-hand side (dependent)
  } deriving stock (Eq, Ord, Show)

-- | Compute the closure of a set of attributes under given FDs
closure :: AttributeSet -> [FD] -> AttributeSet
closure attrs fds = go attrs
  where
    go current
      | current == expanded = current
      | otherwise = go expanded
      where
        expanded = foldl' applyFD current fds
        applyFD acc (FD lhs rhs)
          | lhs `Set.isSubsetOf` acc = acc `Set.union` rhs
          | otherwise = acc

-- | Alias for closure
closureUnderFDs :: [FD] -> AttributeSet -> AttributeSet
closureUnderFDs fds attrs = closure attrs fds

-- | Compute the minimal cover of a set of FDs
-- 1. Decompose RHS to singleton
-- 2. Remove extraneous LHS attributes
-- 3. Remove redundant FDs
getMinimalCover :: [FD] -> [FD]
getMinimalCover fds = removeRedundant $ removeExtraneous $ decompose fds

-- | Decompose FDs to have singleton RHS
decompose :: [FD] -> [FD]
decompose = concatMap splitRhs
  where
    splitRhs (FD lhs rhs) = [FD lhs (Set.singleton r) | r <- Set.toList rhs]

-- | Remove extraneous attributes from LHS of each FD
removeExtraneous :: [FD] -> [FD]
removeExtraneous fds = map removeFromFD fds
  where
    removeFromFD (FD lhs rhs) = FD (minimalLhs lhs rhs) rhs

    minimalLhs :: AttributeSet -> AttributeSet -> AttributeSet
    minimalLhs lhs rhs = foldl' (tryRemove rhs) lhs (Set.toList lhs)

    tryRemove :: AttributeSet -> AttributeSet -> Attribute -> AttributeSet
    tryRemove rhs currentLhs attr
      | Set.size currentLhs == 1 = currentLhs  -- Can't remove last attr
      | rhs `Set.isSubsetOf` closure (Set.delete attr currentLhs) fds = 
          Set.delete attr currentLhs
      | otherwise = currentLhs

-- | Remove redundant FDs
removeRedundant :: [FD] -> [FD]
removeRedundant = go []
  where
    go acc [] = reverse acc
    go acc (fd@(FD lhs rhs):rest)
      | rhs `Set.isSubsetOf` closure lhs (acc ++ rest) = go acc rest
      | otherwise = go (fd : acc) rest
