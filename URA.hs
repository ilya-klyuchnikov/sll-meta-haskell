module URA where

import Data
import DataUtil
import Driving

ura :: Machine Conf -> Conf -> Expr -> [Subst Conf]
ura machine conf answer = traverse [(idContr conf, buildConfTree machine conf)] [] where
    traverse :: [(Subst Conf, Tree Conf)] -> [(Subst Conf, Tree Conf)] -> [Subst Conf]
    traverse [] [] = []
    traverse [] queue' = traverse queue' []
    traverse ((sub, t) : queue) queue' =
        case t of
            Leaf a | a == answer -> sub : traverse queue queue'
            Leaf a | otherwise   -> traverse queue queue'
            Node _ (ETransient _ t') -> traverse queue ((sub, t') : queue')
            Node _ (EVariants variants) -> traverse queue (queue'' ++ queue') where
                queue'' = map (\(contr, t') -> (sub /// contr, t')) variants
