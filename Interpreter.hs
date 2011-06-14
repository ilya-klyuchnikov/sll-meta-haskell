module Interpreter where
import Data
import DataUtil

eval :: Program -> Expr -> Expr
eval p = intEvalTree . buildEvalTree (evalMachine p)

intEvalTree :: Tree Expr -> Expr
intEvalTree (Leaf e) = e
intEvalTree (Node _ (ETransient _ t)) = intEvalTree t
intEvalTree (Node _ (EDecompose comp ts)) = comp (map intEvalTree ts)

buildEvalTree :: Machine Expr -> Expr -> Tree Expr
buildEvalTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildEvalTree m e))
    Decompose comp ds -> Node c (EDecompose comp (map (buildEvalTree m) ds))

evalMachine :: Program -> Machine Expr
evalMachine p = evalStep where
    evalStep :: Machine Expr
    evalStep (Atom n) =
        Stop (Atom n)
    evalStep (Ctr name []) =
        Stop (Ctr name [])
    evalStep (Ctr name args) = 
        Decompose (Ctr name) args
    evalStep (FCall name args) | (FDef _ vs body) <- fDef p name= 
        Transient Nothing (body // zip vs args)
    evalStep (GCall g ((Ctr c cargs) : args)) | (GDef _ pat@(Pat _ cvs) vs body) <- gDef p g c =
        Transient (Just (CtrMatch pat)) (body // zip (cvs ++ vs) (cargs ++ args))
    evalStep (GCall gname (arg:args)) | Transient cond arg' <- evalStep arg = 
        Transient cond (GCall gname (arg':args))
    evalStep (TestEq (e1, e2) branches) | reducible e1, Transient cond e1' <- evalStep e1 =
        Transient cond (TestEq (e1', e2) branches)
    evalStep (TestEq (e1, e2) branches) | reducible e2, Transient cond e2' <- evalStep e2 =
        Transient cond (TestEq (e1, e2') branches)
    evalStep (TestEq (a1, a2) (e1, e2)) 
        | a1 == a2  = Transient (Just (TestRes True)) e1 
        | otherwise = Transient (Just (TestRes False)) e2