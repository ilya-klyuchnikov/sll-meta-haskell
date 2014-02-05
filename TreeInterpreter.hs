module TreeInterpreter where

import Data
import Data.List
import DataUtil
import Data.Maybe
import DataIO

-- is able to interprete a tree of configuration, given an environment
-- assumptions - NO if-expressions
-- this is an interpreter for MIXED "semantics"
intTree :: Tree Expr -> Env -> Expr
intTree (Leaf e) env =
    e // env
intTree (Node (Ctr cname _) (EDecompose name ts)) env =
    Ctr name $ map (\t -> intTree t env) ts
intTree (Node _ (ETransient _ t)) env =
    intTree t env
intTree (Node e (EVariants cs)) env =
    head $ catMaybes $ map (try env) cs

try :: Env -> (Subst Expr, Tree Expr) -> (Maybe Expr)
-- outcome of matching
try env ([(v, ctr@(Ctr pn _))], t) =
    if cn == pn then (Just $ intTree t extEnv) else Nothing where
    	-- evaluate a scrutenee var in current environment
        c@(Ctr cn cargs) = (Var v []) // env
        -- extending environment
        extEnv = zip (vnames ctr) cargs ++ env
-- outcome of if-expression
try env ((v, e2):_, t) =
	if ((Var v [] // env) == (e2 // env)) then (Just $ intTree t env) else Nothing
