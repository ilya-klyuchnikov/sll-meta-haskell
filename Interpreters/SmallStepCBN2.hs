module Interpreters.SmallStepCBN2 where

import Data
import DataUtil

buildTree :: Program -> Expr -> Tree Expr
buildTree p e = buildEvaluationTree (exprMachine p) e

-- only two kinds of edges, since this is true small-step setting
buildEvaluationTree :: Machine Expr -> Expr -> Tree Expr
buildEvaluationTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildEvaluationTree m e))

exprMachine :: Program -> Machine Expr
exprMachine p = step where
    step :: Machine Expr
    step e | isValue e =
        Stop e
    step (Ctr name args) | Transient cond x' <- step x =
        Transient cond (Ctr name (vals ++ (x' : xs))) where
            (vals, x : xs) = span isValue args
    step (FCall name args) | (FDef _ vs body) <- fDef p name=
        Transient Nothing (body // zip vs args)
    step (GCall g ((Ctr c cargs) : args)) | (GDef _ pat@(Pat _ cvs) vs body) <- gDef p g c =
        Transient (Just (CtrMatch pat)) (body // zip (cvs ++ vs) (cargs ++ args))
    step (GCall gname (arg:args)) | Transient cond arg' <- step arg =
        Transient cond (GCall gname (arg':args))
    step (TestEq (e1, e2) branches) | reducible e1, Transient cond e1' <- step e1 =
        Transient cond (TestEq (e1', e2) branches)
    step (TestEq (e1, e2) branches) | reducible e2, Transient cond e2' <- step e2 =
        Transient cond (TestEq (e1, e2') branches)
    step (TestEq cond (e1, e2)) | DefEqual <- test cond =
        Transient (Just (TestRes True)) e1
    step (TestEq cond (e1, e2)) | DefNotEqual <- test cond =
        Transient (Just (TestRes False)) e2

int :: Program -> Expr -> Expr
int p = intEvalTree . buildEvaluationTree (exprMachine p)

intEvalTree :: Tree Expr -> Expr
intEvalTree (Leaf e) = e
intEvalTree (Node _ (ETransient _ t)) = intEvalTree t
