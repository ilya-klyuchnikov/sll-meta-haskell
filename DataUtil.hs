module DataUtil where

import Data
import Maybe
import Char
import List

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
renaming = undefined

nodeLabel :: Node a -> a
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