{-# LANGUAGE OverloadedStrings #-}
{-|
Tests for Logic.Normalizer module
-}
module Logic.NormalizerSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import Data.Text (Text)

import Logic.Attributes
import Logic.Normalizer

-- | Helper to create a simple relation with single-attr FDs
mkRelation :: Text -> [Text] -> [(Text, Text)] -> Relation
mkRelation name attrs fds = Relation name (Set.fromList attrs) (map mkFD fds)
  where
    mkFD (l, r) = FD (Set.singleton l) (Set.singleton r)

-- | Helper for multi-attribute FDs
mkMultiFD :: [Text] -> [Text] -> FD
mkMultiFD lhs rhs = FD (Set.fromList lhs) (Set.fromList rhs)

spec :: Spec
spec = do
  describe "isSuperkey" $ do
    it "returns True for all attributes" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B")]
      isSuperkey (Set.fromList ["A", "B", "C"]) rel `shouldBe` True
    
    it "returns True for key that determines all" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C")]
      isSuperkey (Set.singleton "A") rel `shouldBe` True
    
    it "returns False for non-key" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B")]
      isSuperkey (Set.singleton "B") rel `shouldBe` False

  describe "findCandidateKeys" $ do
    it "finds single-attribute key" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C")]
          keys = findCandidateKeys rel
      keys `shouldBe` [Set.singleton "A"]
    
    it "finds composite key" $ do
      let rel = Relation "R" 
            (Set.fromList ["A", "B", "C"]) 
            [mkMultiFD ["A", "B"] ["C"]]
          keys = findCandidateKeys rel
      keys `shouldContain` [Set.fromList ["A", "B"]]

  describe "isBCNF" $ do
    it "returns True for relation in BCNF" $ do
      -- A is the key, A -> B, A -> C
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C")]
      isBCNF rel `shouldBe` True
    
    it "returns False for BCNF violation" $ do
      -- A is key, but B -> C violates BCNF (B is not superkey)
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C"), ("B", "C")]
      isBCNF rel `shouldBe` False

  describe "is3NF" $ do
    it "returns True for relation in 3NF" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C")]
      is3NF rel `shouldBe` True
    
    it "returns True when RHS is prime attribute" $ do
      -- AB -> C, C -> A (A is prime because it's part of key AB)
      let rel = Relation "R"
            (Set.fromList ["A", "B", "C"])
            [mkMultiFD ["A", "B"] ["C"], FD (Set.singleton "C") (Set.singleton "A")]
      -- Note: This depends on candidate key detection
      is3NF rel `shouldBe` True

  describe "checkHealth" $ do
    it "returns Healthy for BCNF relation" $ do
      let rel = mkRelation "R" ["A", "B"] [("A", "B")]
          health = checkHealth rel
      nhLevel health `shouldBe` Healthy
    
    it "returns Critical for non-3NF relation" $ do
      -- Contrived example with clear violation
      let rel = Relation "R"
            (Set.fromList ["A", "B", "C", "D"])
            [ FD (Set.singleton "A") (Set.fromList ["B", "C", "D"])
            , FD (Set.singleton "B") (Set.singleton "C")  -- B not superkey, C not prime
            ]
          health = checkHealth rel
      nhLevel health `shouldBe` Critical

  describe "decomposeToBCNF" $ do
    it "returns unchanged for BCNF relation" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("A", "C")]
          result = decomposeToBCNF rel
      length (drRelations result) `shouldBe` 1
      drWarnings result `shouldBe` []
    
    it "decomposes non-BCNF relation" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("B", "C")]
          result = decomposeToBCNF rel
      length (drRelations result) `shouldSatisfy` (> 1)

  describe "synthesizeTo3NF" $ do
    it "creates relations for each FD in minimal cover" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("B", "C")]
          result = synthesizeTo3NF rel
      length (drRelations result) `shouldSatisfy` (>= 2)
    
    it "ensures candidate key is preserved" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B")]
          result = synthesizeTo3NF rel
          allAttrs = concatMap (Set.toList . relAttrs) (drRelations result)
      allAttrs `shouldContain` ["A"]

  describe "optimizeForPerformance" $ do
    it "keeps snapshot attributes together" $ do
      let rel = Relation "Orders"
            (Set.fromList ["OrderID", "ProductID", "PriceAtPurchase"])
            [FD (Set.singleton "ProductID") (Set.singleton "PriceAtPurchase")]
          result = optimizeForPerformance rel
      -- Should keep as single table due to "At" pattern
      length (drRelations result) `shouldBe` 1
      drWarnings result `shouldSatisfy` (not . null)
    
    it "falls back to 3NF for non-snapshot patterns" $ do
      let rel = mkRelation "R" ["A", "B", "C"] [("A", "B"), ("B", "C")]
          result = optimizeForPerformance rel
      -- No "At" pattern, should use 3NF
      length (drRelations result) `shouldSatisfy` (>= 1)
