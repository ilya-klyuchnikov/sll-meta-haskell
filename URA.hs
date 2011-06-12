module URA where

import Data
import DataUtil
import Driving

-- This is "trivial" URA - it Doesn't handle constructors yet
-- Assumptions: 
-- 1. conf   :: Atom
-- 2. answer :: Atom
ura :: Machine Conf -> Expr -> Conf -> [Subst]
ura machine answer conf = breadFirstTraversal [(sub0, tree0)] [] where
    sub0 = map (\n -> (n, Var n [])) (vnames conf)
    tree0 = buildTree machine conf
    breadFirstTraversal :: [(Subst, Tree Conf)] -> [(Subst, Tree Conf)] -> [Subst]
    breadFirstTraversal [] [] = []
    breadFirstTraversal [] queue' = breadFirstTraversal queue' []
    breadFirstTraversal ((sub, t) : queue) queue' =
        case t of
            Leaf a | a == answer -> sub : breadFirstTraversal queue queue'
            Leaf a | otherwise   -> breadFirstTraversal queue queue'
            Node _ (ETransient _ t') -> breadFirstTraversal queue ((sub, t') : queue')
            Node _ (EVariants variants) -> breadFirstTraversal queue (queue'' ++ queue') where
                queue'' = map (\(contra, t') -> (sub /// (contra2sub contra), t')) variants