module NeighborhoodAnalysis(nan) where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import Data.List

nan :: Machine Conf -> Conf -> Expr -> Subst Conf
nan m conf expr = nan' (idContr conf) (buildConfTree m conf) (buildExprTree m expr)

nan' :: Subst Conf -> Tree Conf -> Tree Expr -> Subst Conf
nan' contr (Node _ (EDecompose _ cts)) (Node _ (EDecompose _ ets)) = 
    foldl1 intersectContr (zipWith (nan' contr) cts ets)
nan' contr (Node _ (ETransient _ ct)) (Node _ (ETransient _ et)) = 
    nan' contr ct et
nan' contr (Node conf (EVariants vs)) (Node _ (ETransient (Just (CtrMatch (Pat cname _))) et)) = 
    nan' (contr /// contr') ct et where
        [(contr', ct)] = [(c, t) | (c@[(_, Ctr n _)], t) <- vs, n == cname]
nan' contr (Node conf (EVariants [(contr', ct), _])) (Node _ (ETransient (Just (TestRes True)) et)) = 
    nan' (contr /// contr') ct et
nan' contr (Node conf (EVariants [_, (contr', ct)])) (Node _ (ETransient (Just (TestRes False)) et)) = 
    nan' (contr /// contr') ct et
nan' contr  (Leaf (Var n _)) (Leaf e1) =
    contr /// [(n, e1)]
nan' contr _ _ =
    contr
