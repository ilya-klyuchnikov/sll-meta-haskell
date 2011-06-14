module Interpreter where

import Data
import DataUtil

-- it can be seen as "decomposed evaluation"
eval :: Program -> Expr -> Expr
eval p = intEvalTree . buildEvalTree (evalMachine p)

intEvalTree :: Tree Expr -> Expr
intEvalTree (Leaf e) = e
intEvalTree (Node _ (ETransient _ t)) = intEvalTree t
intEvalTree (Node _ (EDecompose comp ts)) = comp (map intEvalTree ts)

-- TODO: rename to Expression Tree
buildEvalTree :: Machine Expr -> Expr -> Tree Expr
buildEvalTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildEvalTree m e))
    Decompose comp ds -> Node c (EDecompose comp (map (buildEvalTree m) ds))

-- TODO: contr, testInfo -> condition
evalMachine :: Program -> Machine Expr
evalMachine p = evalStep where
    evalStep :: Machine Expr
    evalStep (Atom n) =
        Stop (Atom n)
    evalStep (Ctr name []) =
        Stop (Ctr name [])
    evalStep (Ctr name args) = 
        Decompose (Ctr name) args
    evalStep (FCall name args) = 
        Transient Nothing (body // zip vs args) where
            (FDef _ vs body) = fDef p name
    evalStep (GCall gname ((Ctr cname cargs) : args)) = 
        Transient (Just (CtrMatch pat)) (body // sub) where 
            (GDef _ pat@(Pat _ cvs) vs body) = gDef p gname cname
            sub = zip (cvs ++ vs) (cargs ++ args)
    evalStep (GCall gname (arg:args)) | Transient contr arg' <- evalStep arg = 
        Transient contr (GCall gname (arg':args))
    evalStep (TestEq (Atom n1, Atom n2) (e1, e2)) 
        | n1 == n2  = Transient (Just (TestRes True)) e1 
        | otherwise = Transient (Just (TestRes False)) e2
    evalStep (TestEq (Atom n1, e2) branches) | Transient testInfo e2' <- evalStep e2 =
        Transient testInfo (TestEq (Atom n1, e2') branches)
    evalStep (TestEq (e1, e2) branches) | Transient testInfo e1' <- evalStep e1 =
        Transient testInfo (TestEq (e1', e2) branches)
