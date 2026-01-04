{-# LANGUAGE OverloadedStrings #-}
{-|
Tests for Logic.Attributes module
-}
module Logic.AttributesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set

import Logic.Attributes

spec :: Spec
spec = do
  describe "closure" $ do
    it "returns the input for empty FD list" $ do
      let attrs = Set.fromList ["A", "B"]
      closure attrs [] `shouldBe` attrs
    
    it "computes simple closure correctly" $ do
      -- A -> B, B -> C, given {A}, closure should be {A, B, C}
      let fds = [ FD (Set.singleton "A") (Set.singleton "B")
                , FD (Set.singleton "B") (Set.singleton "C")
                ]
      closure (Set.singleton "A") fds `shouldBe` Set.fromList ["A", "B", "C"]
    
    it "handles transitive closure" $ do
      -- A -> B, B -> C, C -> D
      let fds = [ FD (Set.singleton "A") (Set.singleton "B")
                , FD (Set.singleton "B") (Set.singleton "C")
                , FD (Set.singleton "C") (Set.singleton "D")
                ]
      closure (Set.singleton "A") fds `shouldBe` Set.fromList ["A", "B", "C", "D"]
    
    it "handles multi-attribute LHS" $ do
      -- AB -> C
      let fds = [FD (Set.fromList ["A", "B"]) (Set.singleton "C")]
      closure (Set.fromList ["A", "B"]) fds `shouldBe` Set.fromList ["A", "B", "C"]
      closure (Set.singleton "A") fds `shouldBe` Set.singleton "A"
    
    it "handles multi-attribute RHS" $ do
      -- A -> BC
      let fds = [FD (Set.singleton "A") (Set.fromList ["B", "C"])]
      closure (Set.singleton "A") fds `shouldBe` Set.fromList ["A", "B", "C"]

  describe "getMinimalCover" $ do
    it "returns empty for empty input" $ do
      getMinimalCover [] `shouldBe` []
    
    it "decomposes multi-attribute RHS" $ do
      -- A -> BC should become A -> B, A -> C
      let fds = [FD (Set.singleton "A") (Set.fromList ["B", "C"])]
          result = getMinimalCover fds
      length result `shouldBe` 2
      all (\(FD _ rhs) -> Set.size rhs == 1) result `shouldBe` True
    
    it "removes redundant FDs" $ do
      -- A -> B, A -> C, A -> BC (redundant)
      let fds = [ FD (Set.singleton "A") (Set.singleton "B")
                , FD (Set.singleton "A") (Set.singleton "C")
                , FD (Set.singleton "A") (Set.fromList ["B", "C"])  -- redundant
                ]
          result = getMinimalCover fds
      length result `shouldBe` 2
    
    it "removes extraneous LHS attributes" $ do
      -- AB -> C where A -> C already holds
      let fds = [ FD (Set.singleton "A") (Set.singleton "C")
                , FD (Set.fromList ["A", "B"]) (Set.singleton "C")
                ]
          result = getMinimalCover fds
      -- Should reduce AB -> C to just A -> C or remove as redundant
      length result `shouldBe` 1

  describe "closureUnderFDs" $ do
    it "is an alias for closure with swapped args" $ do
      let fds = [FD (Set.singleton "A") (Set.singleton "B")]
          attrs = Set.singleton "A"
      closureUnderFDs fds attrs `shouldBe` closure attrs fds
