module NeighborhoodAnalysis(nan) where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import Data.List

nan :: Machine Conf -> Expr -> Conf -> Subst
nan m e1 e2 = sub where
    sub0 = map (\n -> (n, Var n [])) (vnames e2)
    sub = nan' sub0 (buildEvalTree m e1) (buildTree m e2)

nan' :: Subst -> Tree Expr -> Tree Conf -> Subst
nan' sub (Node _ (EDecompose _ ts1)) (Node _ (EDecompose _ ts2)) = sub' where
    sub' = foldl1 intersectSubst (zipWith (nan' sub) ts1 ts2)
nan' sub (Node _ (ETransient _ t1)) (Node _ (ETransient _ t2)) = 
    nan' sub t1 t2
nan' sub (Node _ (ETransient (Just (CtrMatch (Pat cname _))) t1)) (Node conf (EVariants xs)) = sub' where
    Just (contr, t2) = find (\(Contraction _ (Ctr cn _), _) -> cn == cname) xs
    sub' = nan' (sub /// contra2sub contr) t1 t2
nan' sub (Node _ (ETransient (Just (TestRes res)) t1)) (Node conf (EVariants [c1, c2])) = sub' where
    (contr, t2) = case res of 
        True -> c1
        False -> c2
    sub' = nan' (sub /// contra2sub contr) t1 t2
nan' sub (Leaf e1) (Leaf (Var n _)) = 
    sub /// [(n, e1)]
nan' sub _ _ = 
    sub

intersectSubst :: Subst -> Subst -> Subst
intersectSubst s1 s2 = intersection where
    intersection = zip keys $ zipWith mergeRestrictions vals1' vals2'
    keys  = map fst s1
    vals1 = map snd s1
    vals2 = map snd s2
    sub = mgu $ zip vals1 vals2
    vals1' = map (// sub) vals1 
    vals2' = map (// sub) vals2 

mergeRestrictions :: Conf -> Conf -> Conf
mergeRestrictions (Ctr n1 args1) (Ctr n2 args2) | n1 == n2 =
    Ctr n1 (zipWith mergeRestrictions args1 args2)
mergeRestrictions (Var v1 rs1) (Var v2 rs2) | v1 == v2 =
    Var v1 (union rs1 rs2)
mergeRestrictions e1 e2 | e1 == e2 = 
    e1

mgu :: [(Conf, Conf)] -> Subst
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