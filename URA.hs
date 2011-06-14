module URA where

import Data
import DataUtil
import Driving

ura :: Machine Conf -> Expr -> Conf -> [Subst Conf]
ura machine answer conf = traverse [(sub0, buildTree machine conf)] [] where
    sub0 = map (\n -> (n, var n)) (vnames conf)
    traverse :: [(Subst Conf, Tree Conf)] -> [(Subst Conf, Tree Conf)] -> [Subst Conf]
    traverse [] [] = []
    traverse [] queue' = traverse queue' []
    traverse ((sub, t) : queue) queue' =
        case t of
            Leaf a | a == answer -> sub : traverse queue queue'
            Leaf a | otherwise   -> traverse queue queue'
            Node _ (ETransient _ t') -> traverse queue ((sub, t') : queue')
            Node _ (EVariants variants) -> traverse queue (queue'' ++ queue') where
                queue'' = map (\(contra, t') -> (sub /// (contra2sub contra), t')) variants
