module Driving where

import Data
import DataUtil
import Interpreter

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildTree m e))
    Decompose comp ds -> Node c (EDecompose comp (map (buildTree m) ds))
    Variants cs -> Node c (EVariants [(c, buildTree m e) | (c, e) <- cs])

driveMachine :: Program -> Machine Conf
driveMachine p = step where
    step :: Machine Conf
    step e@(Var _ _) = 
        Stop e
    step (GCall gn args) | isVar (head args) = 
        Variants (map (scrutinize args) (gDefs p gn))
    step (GCall gn (arg:args)) | reducible arg , Variants cs <- step arg = 
        Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
    step (TestEq (e1, e2) bs) | reducible e1, Variants cs <- step e1 = 
        Variants (map (\(c, e1') -> (c, (TestEq (e1', e2) bs))) cs)
    step (TestEq (e1, e2) bs) | reducible e2, Variants cs <- step e2 =
        Variants (map (\(c, e2') -> (c, (TestEq (e1, e2') bs))) cs)
    step (TestEq (Var a1 rs1, Var a2 rs2) (e1, e2)) 
        | a1 == a2               = Transient (Just (TestRes True)) e1 
        | (Var a2 []) `elem` rs1 = Transient (Just (TestRes False)) e2
        | otherwise              = Variants [(s1, e1 // s1), (s2, e2 // s2)] where
            s1 = [(a1, Var a2 rs2)]
            s2 = [(a1, Var a1 [var a2]), (a2, Var a2 [var a1])]
    step (TestEq (Var v rs, a@(Atom _)) (e1, e2)) 
        | a `elem` rs            = Transient (Just (TestRes False)) e2
        | otherwise              = Variants [(s1, e1 // s1), (s2, e2 // s2)] where
            s1 = [(v, a)]
            s2 = [(v, Var v [a])]
    step (TestEq (a@(Atom _), v@(Var _ _)) bs) = step (TestEq (v, a) bs)
    step e = evalMachine p e

perfectDriveMachine :: Program -> Machine Conf
perfectDriveMachine  = (propagateContraction .) . driveMachine

scrutinize ::  [Expr] -> GDef -> (Subst Expr, Expr)
scrutinize ((Var v _) : args) (GDef _ pat@(Pat cn cvs) vs body) = 
    ([(v, Ctr cn fresh)], body // sub) where
        fresh =  makeFreshVars v pat
        sub = zip (cvs ++ vs) (fresh ++ args)

makeFreshVars :: Name -> Pat -> [Expr]
makeFreshVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

propagateContraction :: Step Conf -> Step Conf
propagateContraction (Variants vs) = Variants [(c, e // c) | (c, e) <- vs]
propagateContraction step = step