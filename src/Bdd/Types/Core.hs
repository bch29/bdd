{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
module Bdd.Types.Core where

import           Control.Lens.Plated (Plated)
import           Control.Lens.TH
import           Data.Data
import           Data.Hashable       (Hashable)
import           Data.Vector         (Vector)
import           GHC.Generics        (Generic)

data ExprTree
  = Var Int
  | ExprTree :<-> ExprTree
  | ExprTree :-> ExprTree
  | ExprTree :& ExprTree
  | ExprTree :| ExprTree
  | Not ExprTree
  deriving (Generic, Show, Eq, Data)

infixr 7 :&
infixr 6 :|
infixr 5 :->
infix 4 :<->

data Expr v =
  Expr
  { _exprTree :: ExprTree
  , _exprVars :: Vector v
  } deriving (Generic, Show, Eq)

instance Plated ExprTree where
instance Hashable ExprTree where

data BddTree =
  -- | The integer determines the index of the variable we are testing. Branch
  -- left when the variable is false, right when it's true.
    Branch Int BddTree BddTree
  | Leaf Bool
  deriving (Eq, Data)

instance Plated BddTree where

data Bdd v =
  Bdd
  { _bddTree :: BddTree
  , _bddVars :: Vector v }

makePrisms ''ExprTree
makeLenses ''Expr
makePrisms ''BddTree
makeLenses ''Bdd
