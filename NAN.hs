module NAN (nan) where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import Data.List

nan :: Machine Expr -> Expr -> Expr -> Subst Expr
nan m conf expr = nan' (idContr conf) (buildProcessTree m conf) (buildEvaluationTree m expr)

nan' :: Subst Expr -> Tree Expr -> Tree Expr -> Subst Expr
nan' contr (Node _ (EDecompose _ cts)) (Node _ (EDecompose _ ets)) =
    foldl1 intersectContr (zipWith (nan' contr) cts ets)
nan' contr (Node _ (ETransient _ ct)) (Node _ (ETransient _ et)) =
    nan' contr ct et
nan' contr (Node _ (EVariants vs)) (Node _ (ETransient (Just testResult) et)) =
    nan' (contr /// contr') ct et where (contr', ct) = matchVariant vs testResult
nan' contr (Leaf (Var n _)) (Leaf e) =
    contr /// [(n, e)]
nan' contr _ _ =
    contr

matchVariant :: [(Subst Expr, Tree Expr)] -> TestResult -> (Subst Expr, Tree Expr)
matchVariant v (CtrMatch (Pat cn _)) = head [v' | v'@([(_, Ctr n _)], t) <- v, n == cn]
matchVariant [(contr', ct), _] (TestRes True)  = (contr', ct)
matchVariant [_, (contr', ct)] (TestRes False) = (contr', ct)

-- identity contraction:
-- maps all variables of an expression to itself
idContr conf = map (\n -> (n, var n)) (vnames conf)

intersectContr :: Subst Expr -> Subst Expr -> Subst Expr
intersectContr s1 s2 = intersection where
    intersection = zip keys $ zipWith mergeRestrictions vals1' vals2'
    keys  = map fst s1
    vals1 = map snd s1
    vals2 = map snd s2
    sub = mgu $ zip vals1 vals2
    vals1' = map (// sub) vals1
    vals2' = map (// sub) vals2

mergeRestrictions :: Expr -> Expr -> Expr
mergeRestrictions (Ctr n1 args1) (Ctr n2 args2) | n1 == n2 =
    Ctr n1 (zipWith mergeRestrictions args1 args2)
mergeRestrictions (Var v1 rs1) (Var v2 rs2) | v1 == v2 =
    Var v1 (union rs1 rs2)
mergeRestrictions e1 e2 | e1 == e2 =
    e1

mgu :: [(Expr, Expr)] -> Subst Expr
mgu [] = []
mgu (eq : eqs) =
    case eq of
        (e1, e2) | e1 == e2 -> mgu eqs
        (Ctr n1 args1, Ctr n2 args2) -> mgu ((zip args1 args2) ++ eqs)
        (Var v1 rs1, Var v2 rs2) -> mgu' [(max v1 v2, var (min v1 v2))]
        (Var v1 _, e2) -> mgu' [(v1, e2)]
        (e1, Var v2 _) -> mgu' [(v2, e1)]
    where
        mgu' s = (s /// sub) ++ sub
            where sub = mgu $ map (\(e1, e2) -> (e1 // s, e2 // s)) eqs
