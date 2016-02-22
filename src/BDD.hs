module Bdd where

import           Control.Lens
import           Control.Monad.State (MonadState (..), State, gets, modify,
                                      runState)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as Vec
import           GHC.Generics        (Generic)
import Data.Data
import Data.Data.Lens (uniplate)

import Bdd.Types

--------------------------------------------------------------------------------
--  Environments
--------------------------------------------------------------------------------

type Env = HashMap ExprTree BddTree
type EnvM = State Env

askBdd :: ExprTree -> EnvM (Maybe BddTree)
askBdd = gets . Map.lookup

putBdd :: ExprTree -> BddTree -> EnvM ()
putBdd expr = modify . Map.insert expr

-- | Get the 'Bdd' for an expression, or compute the provided one and use that if
-- none has been computed already.
getOrPutBdd :: ExprTree -> EnvM BddTree -> EnvM BddTree
getOrPutBdd expr ifNotThere =
  askBdd expr >>= maybe (do res <- ifNotThere; putBdd expr res; return res) return

--------------------------------------------------------------------------------
--  Logic
--------------------------------------------------------------------------------

-- | Calculate the BDD for an expression and put it in the environment. Return
-- an existing BDD without doing any work if one is available.
constrPut :: (Hashable v, Eq v) => Expr v -> EnvM (Bdd v)
constrPut expr =
  let normedExpr@(Expr normedTree normedVars) = normalizeExprVars expr
  in Bdd <$> getOrPutBdd normedTree (constr normedExpr) <*> pure normedVars

-- | Calculate the BDD for an expression. Don't put it in the environment. Do
-- not return an existing BDD if one already exists in the environment.
constr :: (Hashable v, Eq v) => Expr v -> EnvM (Bdd v)
constr (Expr (Var v) vars) = return $ Bdd (Branch 0 (Leaf True) (Leaf False)) vars

constr (Expr (Not expr) vars) =
  do inner <- constrPut (Expr expr vars)
     return (swapLeaves inner)

constr (e1 :| e2) =
  do b1 <- constrPut e1
     b2 <- constrPut e2
     undefined

constr (e1 :& e2) =
  do b1 <- constrPut e1
     b2 <- constrPut e2
     undefined

-- -- Implication and equivalence are handled via their equivalent expressions in
-- -- terms of simpler logical constructs.
-- constr (e1 :-> e2) = constrPut (Not e1 :| e2)
-- constr (e1 :<-> e2) = constrPut ((e1 :-> e2) :& (e2 :-> e1))

-- -- | Calculate the BDD for an expression.
-- constructBdd :: (Hashable v, Eq v) => Expr v -> Bdd v
-- constructBdd = fst . (`runState` mempty) . constr

test1 :: Expr Int
test1 = 1 + (2 + 1 * 2 .-> 3)

test2 :: Expr Int
test2 = 2 + (1 + 2 * 1 .-> 3)
