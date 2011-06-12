module Driving where

import Data
import DataUtil
import Interpreter

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m e

bt :: Machine Conf -> Conf -> Tree Conf
bt m c = case m c of
    Transient test e -> Node c $ ETransient test (bt m e)
    Stop e -> Leaf e
    Decompose comp ds -> Node c $ EDecompose comp $ map (bt m) ds
    Variants cs -> Node c $ EVariants xx 
        where xx = [(c, bt m e) | (c, e) <- cs]

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
    driveStep (TestEq (Var a1 rs1, Var a2 rs2) (e1, e2)) | (Var a2 []) `elem` rs1 =
        Transient (Just (TestRes False)) e2
    -- 3) any vars
    driveStep (TestEq (Var a1 rs1, Var a2 rs2) (e1, e2)) = 
        Variants [tBranch, fBranch] where
            tBranch = (Contraction a1 v2,  e1 // [(a1, v2)])
            fBranch = (Contraction a1 v1', e2 // [(a1, v1'), (a2, v2')])
            v1 = Var a1 []
            v2 = Var a2 []
            v1' = Var a1 [v2]
            v2' = Var a2 [v1]
    -- 4) var / atom
    driveStep (TestEq (Var v _, a@(Atom _)) (e1, e2)) = Variants [tBranch, fBranch] where
        tBranch = (Contraction v a,  e1 // [(v, a)])
        fBranch = (Contraction v v', e2 // [(v, v')])
        v' = Var v [a]
    -- 5) atom / var
    driveStep (TestEq (a@(Atom _), Var v _) (e1, e2)) = Variants [tBranch, fBranch] where
        tBranch = (Contraction v a,  e1 // [(v, a)])
        fBranch = (Contraction v v', e2 // [(v, v')])
        v' = Var v [a]
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
perfectDriveMachine  = addPropagation . driveMachine

scrutinize ::  [Expr] -> GDef -> (Contraction Expr, Expr)
scrutinize ((Var v _) : args) (GDef _ pat@(Pat cn cvs) vs body) = 
    (Contraction v (Ctr cn fresh), body // sub) where
        fresh =  makeFreshVars v pat
        sub = zip (cvs ++ vs) (fresh ++ args)

-- here is the biggest simplification
makeFreshVars :: Name -> Pat -> [Expr]
makeFreshVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

addPropagation :: Machine Conf -> Machine Conf
addPropagation m e = propagateContraction (m e)

propagateContraction :: Step Conf -> Step Conf
propagateContraction (Variants vs) = 
  Variants [(c, e // [(v, e')]) | (c@(Contraction v e'), e) <- vs]
propagateContraction step = step

reducible :: Expr -> Bool
reducible (Var _ _) = False
reducible (Atom _) = False
reducible _ = True