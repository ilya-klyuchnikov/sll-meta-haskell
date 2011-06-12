module NeighborhoodAnalysis(nan) where

import Data
import DataUtil
import DataIO
import Driving
import Data.List
-- import Debug.Trace

-- assumptions:
--  1) m constructs a perfect process tree
--  2) e1 is an instance of e2 (e2 < e1)
--  3) (eval p e1) terminates
nan :: Machine Conf -> Expr -> Conf -> Subst
nan m e1 e2 = sub where -- filter (\(k, v) -> not (Var k [] == v)) 
    sub0 = map (\n -> (n, Var n [])) (vnames e2)
    sub = nan' sub0 (buildTree m e1) (buildTree m e2)

nan' :: Subst -> Tree Expr -> Tree Conf -> Subst
--nan' sub (Node e1 _) (Node e2 _) | trace (show sub ++ "\n\t" ++ (show e1) ++  "\n\t" ++ (show e2)) False = undefined
nan' sub (Node _ (EDecompose _ ts1)) (Node _ (EDecompose _ ts2)) = sub' where
    subs = zipWith (nan' sub) ts1 ts2
    sub' = foldl1 intersectSubst subs
nan' sub (Node _ (ETransient _ t1)) (Node _ (ETransient _ t2)) = 
    nan' sub t1 t2
nan' sub (Node _ (ETransient (Just (CtrMatch (Pat cname _))) t1)) (Node conf (EVariants xs)) = sub' where
    Just (contra, t2) = find (\(Contraction _ (Ctr cn _), _) -> cn == cname) xs
    sub' = nan' (sub /// contra2sub contra) t1 t2
nan' sub (Node _ (ETransient (Just (TestRes res)) t1)) (Node conf (EVariants [c1, c2])) = sub' where
    (contra, t2) = case res of 
        True -> c1
        False -> c2
    sub' = nan' (sub /// contra2sub contra) t1 t2
nan' sub (Leaf e1) (Leaf (Var n _)) = 
    sub /// [(n, e1)]
nan' sub _ _ = 
    sub

-- !!!! by design k1 == k2
intersectSubst :: Subst -> Subst -> Subst
--intersectSubst s1 s2 | trace ("???" ++ show s1 ++ show s2 ++ "???") False = undefined 
--intersectSubst s1 s2 = zipWith (\(k1, val1) (k2, val2) -> (k1, merge val1 val2)) s1 s2
intersectSubst s1 s2 = zipWith (\(v1, e1) (v2, e2) -> (v1, merge e1 e2)) (s1 /// s) (s2 /// s) where
    s = mgu $ zipWith (\(_, v1) (_, v2) -> (v1, v2)) s1 s2

merge :: Conf -> Conf -> Conf
merge (Ctr n1 args1) (Ctr n2 args2) | n1 == n2 =
    Ctr n1 (zipWith merge args1 args2)
merge (Var v1 rs1) (Var v2 rs2) | v1 == v2 =
    Var v1 (union rs1 rs2)
merge e1 e2 | e1 == e2 = 
    e1

-- this is a "positive mgu"
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