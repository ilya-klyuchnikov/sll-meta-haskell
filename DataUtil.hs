module DataUtil where

import Data
import Data.Maybe
import Data.List

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args
isValue (Atom _) = True
isValue _ = False

-- "factory" for simple vars
var :: Name -> Expr
var n = Var n []

-- is a given expression a call?
isCall :: Expr -> Bool
isCall (FCall _ _) = True
isCall (GCall _ _) = True
isCall _ = False

-- is a given expression an "if-expression"?
isTest :: Expr -> Bool
isTest (TestEq _ _) = True
isTest _ = False

-- is a given expression a variable?
isVar :: Expr -> Bool
isVar (Var _ _) = True
isVar _ = False

-- gets an f-definition from a program by name
fDef :: Program -> Name -> FDef
fDef (Program fs _) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

-- gets all g-definitiona from a program by name
gDefs :: Program -> Name -> [GDef]
gDefs (Program _ gs) gname = [g | g@(GDef x _ _ _) <- gs, x == gname]

-- gets a (single!) g-definition by name and pattern
gDef :: Program -> Name -> Name -> GDef
gDef p gname cname = head [g | g@(GDef _ (Pat c _) _ _) <- gDefs p gname, c == cname]

-- applies a substitution to an expression
(//) :: Expr -> Subst Expr -> Expr
Ctr name args            // sub = Ctr   name (map (// sub) args)
FCall name args          // sub = FCall name (map (// sub) args)
GCall name args          // sub = GCall name (map (// sub) args)
Atom x                   // sub = Atom x
TestEq (a1, a2) (e1, e2) // sub = (TestEq (a1 // sub, a2 // sub) (e1 // sub, e2 // sub))
Var x1 []                // sub = fromMaybe (Var x1 []) (lookup x1 sub)
-- more general substitution - a variable with restrictions
-- bookkeeping of restrictions
Var x1 rs1               // sub = case (lookup x1 sub) of
    Nothing -> Var x1 rs1'
    Just (Var x2 rs2) -> Var x2 (union rs1' rs2)
    Just e -> e
    where
        rs1' = nub $ map (clear . (// sub)) rs1
        clear (Var x _) = var x
        clear e = e

-- composition of substitutions
(///) :: Subst Expr -> Subst Expr -> Subst Expr
(///) sub1 sub2 = map (\(k, v) -> (k, v // sub2)) sub1

-- variable names of an expression (without duplicates)
vnames :: Expr -> [Name]
vnames = nub . vnames'

-- boilerplate of getting all names of variables (with possible duplicates)
vnames' :: Expr -> [Name]
vnames' (Var v _) = [v]
vnames' (Atom a) = []
vnames' (Ctr _ args)   = (concat . map vnames') args
vnames' (FCall _ args) = (concat . map vnames') args
vnames' (GCall _ args) = (concat . map vnames') args
vnames' (TestEq (a1, a2) (e1, e2)) = (concat . map vnames') [a1, a2, e1, e2]

-- can we reduce a top-level expression?
reducible :: Expr -> Bool
reducible (FCall _ _)  = True
reducible (GCall _ _)  = True
reducible (TestEq _ _) = True
reducible _            = False

-- TODO: simplify by making new datatype
-- Left True - if expressions are definitely equal
-- Left False - if expressions are definitely not equal
-- Right (sub1, sub2)
--   (sub1 - substitution which results in equality)
--   (sub2 - substitution whuch results in non-equality)
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
