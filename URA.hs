module URA where

import Data
import DataUtil
import Driving

-- finds all possible substitution for IN configuration (`conf`)
-- given an OUT value (`answer`)
ura :: Machine Conf -> Conf -> Expr -> [Subst Conf]
ura machine conf answer = traverse [(idContr conf, buildConfTree machine conf)] where
    traverse :: [(Subst Conf, Tree Conf)] -> [Subst Conf]
    traverse [] = []
    traverse ((sub, t) : queue) =
        case t of
            Leaf a | a == answer -> sub : traverse queue
            Leaf a | otherwise   -> traverse queue
            Node _ (ETransient _ t') -> traverse (queue ++ [(sub, t')])
            Node _ (EVariants variants) -> traverse (queue ++ queue') where
                queue' = map (\(contr, t') -> (sub /// contr, t')) variants
