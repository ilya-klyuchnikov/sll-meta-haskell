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
driveMachine p = driveStep where
    driveStep :: Machine Conf
    driveStep e@(Var _ _) = 
        Stop e
    driveStep (GCall gn args) | isVar (head args) = 
        Variants vars where
            vars = (map (scrutinize args) (gDefs p gn))
    driveStep (GCall gn (arg:args)) | isCall arg || isTest arg = 
        case (driveStep arg) of
            Transient pat t -> Transient pat (GCall gn (t:args))
            Variants cs -> Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
    -- 1) equal vars -- 
    -- this step is not correct for supercompilation (due to generalization)
    driveStep (TestEq (Var a1 _, Var a2 _) (e1, _)) | a1 == a2 =
        Transient (Just (TestRes True)) e1
    -- 2) different vars
    -- this step is not correct for supercompilation (due to generalization)
    driveStep (TestEq (Var a1 rs1, Var a2 rs2) (e1, e2)) | (Var a2 []) `elem` rs1 =
        Transient (Just (TestRes False)) e2
    -- 3) any vars
    driveStep (TestEq (v1@(Var a1 _), v2@(Var a2 _)) (e1, e2)) = 
        Variants [(tSub, e1 // tSub), (fSub, e2 // fSub)] where
            tSub = [(a1, v2)] -- min max??
            fSub = [(a1, Var a1 [var a2]), (a2, Var a2 [var a1])]
    -- 4) var / atom
    driveStep (TestEq (Var v rs, a@(Atom _)) (e1, e2)) | a `elem` rs =
        Transient (Just (TestRes False)) e2
    driveStep (TestEq (Var v _, a@(Atom _)) (e1, e2)) = 
        Variants [(tSub, e1 // tSub), (fSub, e2 // fSub)] where
            tSub = [(v, a)]
            fSub = [(v, Var v [a])]
    -- 5) atom / var
    driveStep (TestEq (a@(Atom _), Var v rs) (e1, e2)) | a `elem` rs = 
        Transient (Just (TestRes False)) e2
    driveStep (TestEq (a@(Atom _), Var v _) (e1, e2)) = 
        Variants [(tSub, e1 // tSub), (fSub, e2 // fSub)] where
            tSub = [(v, a)]
            fSub = [(v, Var v [a])]
    -- 6) reducible / e --
    driveStep (TestEq (e1, e2) bs) | reducible e1 = 
        case (driveStep e1) of
            Transient trInto e1' -> Transient trInto (TestEq (e1', e2) bs)
            Variants cs -> Variants (map (\(c, e1') -> (c, (TestEq (e1', e2) bs))) cs)
    -- 7) e / reducible
    driveStep (TestEq (e1, e2) bs) | reducible e2 = 
        case (driveStep e2) of
            Transient trInto e2' -> Transient trInto (TestEq (e1, e2') bs)
            Variants cs -> Variants (map (\(c, e2') -> (c, (TestEq (e1, e2') bs))) cs)
    -- 8) atom / atom + everything else
    driveStep e =
        evalStep e where evalStep = evalMachine p

perfectDriveMachine :: Program -> Machine Conf
perfectDriveMachine  = (propagateContraction .) . driveMachine

scrutinize ::  [Expr] -> GDef -> (Subst Expr, Expr)
scrutinize ((Var v _) : args) (GDef _ pat@(Pat cn cvs) vs body) = 
    ([(v, Ctr cn fresh)], body // sub) where
        fresh =  makeFreshVars v pat
        sub = zip (cvs ++ vs) (fresh ++ args)

-- here is the biggest simplification
makeFreshVars :: Name -> Pat -> [Expr]
makeFreshVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

propagateContraction :: Step Conf -> Step Conf
propagateContraction (Variants vs) = Variants [(c, e // c) | (c, e) <- vs]
propagateContraction step = step

reducible :: Expr -> Bool
reducible (Var _ _) = False
reducible (Atom _) = False
reducible _ = True