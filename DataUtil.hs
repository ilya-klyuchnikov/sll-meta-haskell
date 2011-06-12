module DataUtil(
    isValue,isCall,isVar,size,
    fDef, gDef, gDefs,
    (//), (///), contra2sub, renaming, vnames,nameSupply,freshVars,
    nodeLabel,isRepeated,unused,isTest,
    pat2Ctr
    ) where
    
import Data
import Maybe
import Char
import List

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args 
isValue _ = False

isCall :: Expr -> Bool
isCall (FCall _ _) = True
isCall (GCall _ _) = True
isCall _ = False

isTest :: Expr -> Bool
isTest (TestEq _ _) = True
isTest _ = False

isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _ = False

fDef :: Program -> Name -> FDef
fDef (Program fs _) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

gDefs :: Program -> Name -> [GDef]
gDefs (Program _ gs) gname = [g | g@(GDef x _ _ _) <- gs, x == gname]

gDef :: Program -> Name -> Name -> GDef
gDef p gname cname = head [g | g@(GDef _ (Pat c _) _ _) <- gDefs p gname, c == cname]

(//) :: Expr -> Subst -> Expr

(Var x rs) // sub = maybe v' sub' (lookup x sub) where
    sub' (Var x1 rs1) = Var x1 rs'' where
        -- apply sub to restrictions
        rs' = map (rSub sub) rs
        rs'' = union rs' rs1
    sub' e = e
    v' = Var x (map (clear . (//sub)) rs)
(Ctr name args) // sub = Ctr name (map (// sub) args)
(FCall name args) // sub = FCall name (map (// sub) args)
(GCall name args) // sub = GCall name (map (// sub) args)
(Atom x) // sub = (Atom x)
(TestEq (a1, a2) (e1, e2)) // sub = (TestEq (a1 // sub, a2 // sub) (e1 // sub, e2 // sub))

-- for restrictions --
rSub :: Subst -> Expr -> Expr
rSub sub e = clear (e // sub) where
clear (Var x _) = Var x []
clear e = e

-- handling a != b contraction (in Scala: implicits)
contra2sub :: Contraction Expr -> Subst
contra2sub (Contraction v1' (Var v1 [Var v2 _])) = [(v1, Var v1 [Var v2 []]), (v2, Var v2 [Var v1 []])]
contra2sub (Contraction v e) = [(v, e)]

(///) :: Subst -> Subst -> Subst
(///) sub1 sub2 = map (\(k, v) -> (k, v // sub2)) sub1

nameSupply :: NameSupply
nameSupply = ["" ++ (show i) | i <- [1 ..] ]

freshVars :: [Expr]
freshVars = [Var ("c." ++ (show i)) [] | i <- [1 ..] ]

unused :: Contraction Conf -> NameSupply -> NameSupply
unused (Contraction _ e) = (\\ vnames e)

vnames :: Expr -> [Name]
vnames = nub . vnames'

vnames' :: Expr -> [Name]
vnames' (Var v _) = [v]
vnames' (Ctr _ args)   = concat $ map vnames' args
vnames' (FCall _ args) = concat $ map vnames' args
vnames' (GCall _ args) = concat $ map vnames' args
vnames' (Atom a) = []

isRepeated :: Name -> Expr -> Bool
isRepeated vn e = (length $ filter (== vn) (vnames' e)) > 1

renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ renaming' (e1, e2) where
    f (x:_, _) = Nothing
    f (_, ps) = g gs1 gs2
        where 
            gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
            gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy h $ nub $ catMaybes ps
            h (a, b) (c, d) = compare a c
    g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys 
        then Just (concat xs) else Nothing

renaming' :: (Expr, Expr) -> [Maybe (Name, Name)]
renaming' ((Var x _), (Var y _)) = [Just (x, y)]
renaming' ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' _  = [Nothing]

size :: Expr -> Integer
size (Var _ _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)

nodeLabel :: Node a -> a
nodeLabel (Node l _) = l
nodeLabel (Leaf l) = l

pat2Ctr :: Pat -> Expr
pat2Ctr (Pat cn vs) = Ctr cn (map (\x -> Var x []) vs)