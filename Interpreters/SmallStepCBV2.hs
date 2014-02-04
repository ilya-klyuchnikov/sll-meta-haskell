module Interpreters.SmallStepCBV2 where

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
    step (FCall name args) | and (map isValue args) =
        Transient Nothing (body // zip vs args) where
            (FDef _ vs body) = fDef p name
    step (FCall name args) | Transient cond x' <- step x =
        Transient cond (FCall name (vals ++ (x' : xs))) where
            (vals, x : xs) = span isValue args
    step (GCall g args) | and (map isValue args) =
        Transient (Just (CtrMatch pat)) body' where
            Ctr cname cargs' : args' = args
            (GDef _ pat@(Pat n cvs) vs body) = gDef p g cname
            body' = body // zip (cvs ++ vs) (cargs' ++ args')
    step (GCall name args) | Transient cond x' <- step x =
        Transient cond (GCall name (vals ++ (x' : xs))) where
            (vals, x : xs) = span isValue args
    step (TestEq (Atom a1, Atom a2) (e1, e2)) | a1 == a2 =
        Transient (Just (TestRes True)) e1
    step (TestEq (Atom a1, Atom a2) (e1, e2)) | a1 /= a2 =
        Transient (Just (TestRes False)) e2
    step (TestEq (a1, Atom a2) (e1, e2))  =
        Transient cond (TestEq (a1', Atom a2) (e1, e2)) where
            Transient cond a1' = step a1
    step (TestEq (Atom a1, a2) (e1, e2))  =
        Transient cond (TestEq (Atom a1, a2') (e1, e2)) where
            Transient cond a2' = step a2

int :: Program -> Expr -> Expr
int p = intEvalTree . buildEvaluationTree (exprMachine p)

intEvalTree :: Tree Expr -> Expr
intEvalTree (Leaf e) = e
intEvalTree (Node _ (ETransient _ t)) = intEvalTree t
