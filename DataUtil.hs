module DataUtil(
    isValue,isCall,isVar,size,var,
    fDef, gDef, gDefs,
    (//), (///), contra2sub, renaming, vnames,nameSupply,freshVars,
    nodeLabel,isRepeated,unused,isTest,
    pat2Ctr
    ) where
    
import Data
import Maybe
import Char
import List

var :: Name -> Expr
var n = Var n []

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
(Var x1 rs1) // sub = case (lookup x1 sub) of
    Nothing -> Var x1 rs1'
    Just (Var x2 rs2) -> Var x2 (union rs1' rs2)
    Just e -> e
    where 
        rs1' = nub $ map (clear . (// sub)) rs1
        clear (Var x _) = var x
        clear e = e
(Ctr name args) // sub = Ctr name (map (// sub) args)
(FCall name args) // sub = FCall name (map (// sub) args)
(GCall name args) // sub = GCall name (map (// sub) args)
(Atom x) // sub = (Atom x)
(TestEq (a1, a2) (e1, e2)) // sub = (TestEq (a1 // sub, a2 // sub) (e1 // sub, e2 // sub))

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

-- TODO
renaming :: Expr -> Expr -> Maybe Renaming
renaming = undefined

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