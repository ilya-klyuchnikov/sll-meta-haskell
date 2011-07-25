module Generator where

import Data
import DataUtil

residuate :: Graph Conf -> Task
residuate (Leaf c) = (c, Program [] [])
residuate (Node c (ETransient _ n1)) = (fcall, p') where
    args = vnames c
    f = genId c
    fcall = FCall f (map var args)
    (body, Program fs gs) = residuate n1
    p' = Program (fdef : fs) gs
    fdef = FDef f args body
residuate (Node _ (EDecompose f ns)) = (f args, p) where
    (args, ps) = unzip (map residuate ns)
    p = foldl merge (Program [] []) ps
residuate (Node c (EVariants vars)) | isPattern vars = (gcall, p') where
    args = vnames c
    g = genId c
    gcall = GCall g (map var args)
    p' = undefined
residuate (Node _ (EFold (Node c1 (EVariants _)) ren)) = (gcall, Program [] []) where
    g = genId c1
    args = map var (vnames c1)
    gcall = (GCall g args) // [(x, var y) | (x, y) <- ren] 
residuate (Node _ (EFold (Node c1 _) ren)) = (fcall, Program [] []) where
    f = genId c1
    args = map var (vnames c1)
    fcall = (FCall f args) // [(x, var y) | (x, y) <- ren] 
    
genId :: Conf -> Name
genId = undefined

isPattern [([(_, Ctr _ _), _], _)] = True
isPattern _ = False