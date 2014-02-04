module Driving where

import Data
import DataUtil
import GenVar
import Interpreter

buildProcessTree :: Machine Expr -> Expr -> Tree Expr
buildProcessTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildProcessTree m e))
    Decompose comp ds -> Node c (EDecompose comp (map (buildProcessTree m) ds))
    Variants cs -> Node c (EVariants [(c, buildProcessTree m e) | (c, e) <- cs])

confMachine :: Program -> Machine Expr
confMachine p = step where
    step :: Machine Expr
    step (GCall gn args) | isVar (head args) =
        Variants (map (scrutinize args) (gDefs p gn))
    step (GCall gn (arg:args)) | reducible arg , Variants cs <- step arg =
        Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
    step (TestEq (e1, e2) bs) | reducible e1, Variants cs <- step e1 =
        Variants (map (\(c, e1') -> (c, (TestEq (e1', e2) bs))) cs)
    step (TestEq (e1, e2) bs) | reducible e2, Variants cs <- step e2 =
        Variants (map (\(c, e2') -> (c, (TestEq (e1, e2') bs))) cs)
    step (TestEq cond (e1, e2)) | CondEqual (s1, s2) <- test cond =
        Variants [(s1, e1 // s1), (s2, e2 // s2)]
    step e@(Var _ _) = Stop e
    step e = exprMachine p e

perfectDriveMachine :: Program -> Machine Expr
perfectDriveMachine  = (propagateContraction .) . confMachine

scrutinize ::  [Expr] -> GDef -> (Subst Expr, Expr)
scrutinize ((Var v _) : args) (GDef _ pat@(Pat cn cvs) vs body) =
    ([(v, Ctr cn fresh)], body // sub) where
        fresh =  genVars v pat
        sub = zip (cvs ++ vs) (fresh ++ args)

propagateContraction :: Step Expr -> Step Expr
propagateContraction (Variants vs) = Variants [(c, e // c) | (c, e) <- vs]
propagateContraction step = step
