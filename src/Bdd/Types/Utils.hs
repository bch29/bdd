{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bdd.Types.Utils (normalizeExprVars, var, (.&), (.|), (.->), (.<->), not', swapLeaves) where

import Bdd.Types.Core

import           Control.Lens
import           Data.Hashable     (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet      as Set
import           Data.Vector       (Vector, (!))
import qualified Data.Vector       as Vec

data CombinedVarMap v =
  CombinedVarMap
  { newVars     :: Vector v -- | The new int -> variable mapping
  , leftVarMap  :: Vector Int -- | Mapping from old left variable indices to new
  , rightVarMap :: Vector Int -- | Mapping from old right variable indices to new
  }

reverseLookupMap
  :: (Eq k, Hashable k) => Vector k -> HashMap k Int
reverseLookupMap = Vec.ifoldr (flip Map.insert) mempty

combineVarLists :: (Eq v, Hashable v) => Vector v -> Vector v -> CombinedVarMap v
combineVarLists leftVars rightVars = CombinedVarMap{..}
  where
    newVars = Vec.fromList (Set.toList newVarSet)
    leftVarMap = fmap ((Map.!) newVarIndices) leftVars
    rightVarMap = fmap ((Map.!) newVarIndices) rightVars

    vecToSet = Vec.foldr Set.insert mempty
    newVarSet = vecToSet leftVars `Set.union` vecToSet rightVars

    newVarIndices = reverseLookupMap newVars

generateVarRemap :: ExprTree -> Vector Int
generateVarRemap = Vec.fromList . orderPreservingNub . collectVarsInOrder
  where
    orderPreservingNub = go mempty
      where
        go seenSet (v : vars) =
          if Set.member v seenSet
          then go seenSet vars
          else v : go (Set.insert v seenSet) vars
        go _ _ = []
    collectVarsInOrder = go
      where
        go (Var v) = [v]
        go (e1 :& e2) = go e1 ++ go e2
        go (e1 :| e2) = go e1 ++ go e2
        go (e1 :-> e2) = go e1 ++ go e2
        go (e1 :<-> e2) = go e1 ++ go e2
        go (Not e) = go e

updateTreeVars :: Vector Int -> ExprTree -> ExprTree
updateTreeVars v = transform (over _Var (v !))

{-|
Remap an expression's variables to a normal order. That is, variables are
ordered by their position in the expression tree. Given two isomorphic
expressions @e1@ and @e2@, we have

@
(normalizeExprVars e1) ^. exprTree = (normalizeExprVars e2) ^. exprTree
@
-}
normalizeExprVars :: (Eq v, Hashable v) => Expr v -> Expr v
normalizeExprVars expr = expr & exprVars .~ fmap ((expr ^. exprVars) !) remap
                              & exprTree %~ updateTreeVars remap
  where
    remap = generateVarRemap (expr ^. exprTree)

combineExprs :: (Eq v, Hashable v) => (ExprTree -> ExprTree -> ExprTree) -> Expr v -> Expr v -> Expr v
combineExprs combineTrees e1 e2 =
  let CombinedVarMap{..} = combineVarLists (e1 ^. exprVars) (e2 ^. exprVars)
      newTree = combineTrees (updateTreeVars leftVarMap (e1 ^. exprTree))
                             (updateTreeVars rightVarMap (e2 ^. exprTree))
  in normalizeExprVars $ Expr newTree newVars

var :: v -> Expr v
var v = Expr (Var 0) [v]

(.&), (.|), (.->), (.<->) :: (Eq v, Hashable v) => Expr v -> Expr v -> Expr v
(.&) = combineExprs (:&)
(.|) = combineExprs (:|)
(.->) = combineExprs (:->)
(.<->) = combineExprs (:<->)

not' :: Expr v -> Expr v
not' = over exprTree Not

instance (Num v, Eq v, Hashable v) => Num (Expr v) where
  fromInteger = var . fromInteger
  (+) = (.|)
  (*) = (.&)
  negate = not'
  signum = error "signum: unsupported operation on Expr"
  abs = error "abs: unsupported operation on Expr"

swapLeaves :: Bdd v -> Bdd v
swapLeaves = transformOn bddTree (over _Leaf not)
