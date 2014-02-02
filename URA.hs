module URA (ura) where

import Data
import DataUtil
import Driving

-- finds all possible substitution for IN configuration (`conf`)
-- given an OUT value (`answer`)
-- Here we assume that we **WILL NOT** encounter decompose steps.
ura :: Machine Expr -> Expr -> Expr -> [Subst Expr]
ura machine conf answer = traverse [(idContr conf, buildProcessTree machine conf)] where
    traverse :: [(Subst Expr, Tree Expr)] -> [Subst Expr]
    traverse [] = []
    traverse ((sub, t) : queue) =
        case t of
            Leaf a | a == answer -> sub : traverse queue
            Leaf a | otherwise   -> traverse queue
            Node _ (ETransient _ t') -> traverse (queue ++ [(sub, t')])
            Node _ (EVariants variants) -> traverse (queue ++ queue') where
                queue' = map (\(contr, t') -> (sub /// contr, t')) variants

-- identity contraction:
-- maps all variables of an expression to itself
idContr conf = map (\n -> (n, var n)) (vnames conf)
