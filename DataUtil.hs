module DataUtil where

import Data
import Data.Maybe
import Data.Char
import Data.List

var :: Name -> Expr
var n = Var n []

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

(//) :: Expr -> Subst Expr -> Expr
Ctr name args            // sub = Ctr   name (map (// sub) args)
FCall name args          // sub = FCall name (map (// sub) args)
GCall name args          // sub = GCall name (map (// sub) args)
Atom x                   // sub = Atom x
TestEq (a1, a2) (e1, e2) // sub = (TestEq (a1 // sub, a2 // sub) (e1 // sub, e2 // sub))
Var x1 rs1               // sub = case (lookup x1 sub) of
    Nothing -> Var x1 rs1'
    Just (Var x2 rs2) -> Var x2 (union rs1' rs2)
    Just e -> e
    where 
        rs1' = nub $ map (clear . (// sub)) rs1
        clear (Var x _) = var x
        clear e = e

(///) :: Subst Expr -> Subst Expr -> Subst Expr
(///) sub1 sub2 = map (\(k, v) -> (k, v // sub2)) sub1

nameSupply :: NameSupply
nameSupply = ["" ++ (show i) | i <- [1 ..] ]

freshVars :: [Expr]
freshVars = [Var ("c." ++ (show i)) [] | i <- [1 ..] ]

vnames :: Expr -> [Name]
vnames = nub . vnames'

vnames' :: Expr -> [Name]
vnames' (Var v _) = [v]
vnames' (Atom a) = []
vnames' (Ctr _ args)   = (concat . map vnames') args
vnames' (FCall _ args) = (concat . map vnames') args
vnames' (GCall _ args) = (concat . map vnames') args
vnames' (TestEq (a1, a2) (e1, e2)) = (concat . map vnames') [a1, a2, e1, e2]

isRepeated :: Name -> Expr -> Bool
isRepeated vn e = (length $ filter (== vn) (vnames' e)) > 1

-- TODO
renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 | skel e1 == skel e2 = if can e2 == can (e1 // sub) then Just (nub ren) else Nothing where
    ren = zip (vnames' e1) (vnames' e2)
    sub = zip (vnames' e1) (map (\v -> Var v[]) (vnames' e2))
renaming e1 e2 | otherwise          = Nothing

skel :: Expr -> Expr
skel (Var _ _) = Var "$" []
skel (Atom a) = (Atom a)
skel (Ctr c args)   = Ctr c (map skel args)
skel (FCall f args) = FCall f (map skel args)
skel (GCall g args) = GCall g (map skel args)
skel (TestEq (a1, a2) (e1, e2)) = (TestEq (skel a1, skel a2) (skel e1, skel e2))

can :: Expr -> Expr
can (Var v rs) = Var v (sort rs)
can (Atom a) = (Atom a)
can (Ctr c args)   = Ctr c (map can args)
can (FCall f args) = FCall f (map can args)
can (GCall g args) = GCall g (map can args)
can (TestEq (a1, a2) (e1, e2)) = (TestEq (can a1, can a2) (can e1, can e2))

nodeLabel :: Tree a -> a
nodeLabel (Node l _) = l
nodeLabel (Leaf l) = l

pat2Ctr :: Pat -> Expr
pat2Ctr (Pat cn vs) = Ctr cn (map (\x -> Var x []) vs)

delim = '.'

prettyName :: Name -> Name
prettyName n | delim `elem` n = pn ++ [delim] ++ (show i) where
    parts = filter (/= [delim]) $ groupBy (\c1 c2 -> c1 /= delim && c2 /= delim) n
    parts' = init parts
    pn = last parts
    i = sum $ zipWith (\i n -> (read i :: Int) * n) parts' [1 ..]
prettyName n = n

prettyVar :: Expr -> Expr
prettyVar (Var v rs) = Var (prettyName v) (map prettyVar rs)
prettyVar (Ctr n args)   = Ctr n (map prettyVar args)
prettyVar (FCall n args) = FCall n (map prettyVar args)
prettyVar (GCall n args) = GCall n (map prettyVar args)
prettyVar (TestEq (a1, a2) (e1, e2)) = TestEq (prettyVar a1, prettyVar a2) (prettyVar e1, prettyVar e2)
prettyVar e = e

reducible :: Expr -> Bool
reducible (FCall _ _)  = True
reducible (GCall _ _)  = True
reducible (TestEq _ _) = True
reducible _            = False

test :: (Expr, Expr) -> Either Bool (Subst Expr, Subst Expr)
test (Var a1 rs1, Var a2 rs2)
    | a1 == a2 = Left True
    | (Var a2 []) `elem` rs1 = Left False
    | otherwise = Right (s1, s2) where
        s1 = [(a1, Var a2 rs2)]
        s2 = [(a1, Var a1 [var a2]), (a2, Var a2 [var a1])]
test (Var v rs, a@(Atom _))
    | a `elem` rs = Left False
    | otherwise   = Right (s1, s2) where
        s1 = [(v, a)]
        s2 = [(v, Var v [a])]
test (a@(Atom _), Var v rs)
    | a `elem` rs = Left False
    | otherwise   = Right (s1, s2) where
        s1 = [(v, a)]
        s2 = [(v, Var v [a])]
test (a1@(Atom _), a2@(Atom _)) =
    Left (a1 == a2)

intersectContr :: Subst Expr -> Subst Expr -> Subst Expr
intersectContr s1 s2 = intersection where
    intersection = zip keys $ zipWith mergeRestrictions vals1' vals2'
    keys  = map fst s1
    vals1 = map snd s1
    vals2 = map snd s2
    sub = mgu $ zip vals1 vals2
    vals1' = map (// sub) vals1
    vals2' = map (// sub) vals2

mergeRestrictions :: Expr -> Expr -> Expr
mergeRestrictions (Ctr n1 args1) (Ctr n2 args2) | n1 == n2 =
    Ctr n1 (zipWith mergeRestrictions args1 args2)
mergeRestrictions (Var v1 rs1) (Var v2 rs2) | v1 == v2 =
    Var v1 (union rs1 rs2)
mergeRestrictions e1 e2 | e1 == e2 =
    e1

mgu :: [(Expr, Expr)] -> Subst Expr
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

makeFreshVars :: Name -> Pat -> [Expr]
makeFreshVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

idContr conf = map (\n -> (n, var n)) (vnames conf)
merge (Program fs1 gs1) (Program fs2 gs2) = Program (fs1 ++ fs2) (gs1 ++ gs2)
