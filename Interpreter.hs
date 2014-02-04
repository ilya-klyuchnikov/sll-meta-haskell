module Interpreter where
import Data
import DataUtil

buildEvaluationTree :: Machine Expr -> Expr -> Tree Expr
buildEvaluationTree m c = case m c of
    Stop e -> Leaf e
    Transient test e -> Node c (ETransient test (buildEvaluationTree m e))
    Decompose name ds -> Node c (EDecompose name (map (buildEvaluationTree m) ds))

exprMachine :: Program -> Machine Expr
exprMachine p = step where
    step :: Machine Expr
    step (Atom n) =
        Stop (Atom n)
    step (Ctr name []) =
        Stop (Ctr name [])
    step (Ctr name args) =
        Decompose name args
    step (FCall name args) | (FDef _ vs body) <- fDef p name =
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

eval :: Program -> Expr -> Expr
eval p = intExprTree . buildEvaluationTree (exprMachine p)

intExprTree :: Tree Expr -> Expr
intExprTree (Leaf e) = e
intExprTree (Node _ (ETransient _ t)) = intExprTree t
intExprTree (Node _ (EDecompose name ts)) = Ctr name (map intExprTree ts)
