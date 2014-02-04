module GenVar (genVars, prettyVar, prettyName) where

import Data
import Data.List

-- "Fresh" names generation
delim = '.'

-- a trick to avoid explicit name generator
-- genVars x (Cons a b) => [1.x, 2.x]
genVars :: Name -> Pat -> [Expr]
genVars n (Pat _ vs) = [Var (show i ++ [delim] ++ n) [] | i <- [1 .. length vs]]

-- makes generated variables more readable
-- maps names in the forms 1.2.3.x to x.14
-- it guaranties uniqueness of names
prettyName :: Name -> Name
prettyName n | delim `elem` n = pn ++ [delim] ++ (show i) where
    -- parts "1.2.3.x" => ["1","2","3","x"]
    parts = filter (/= [delim]) $ groupBy (\c1 c2 -> c1 /= delim && c2 /= delim) n
    -- [1,2,3]
    parts' :: [Int]
    parts' = map read (init parts)
    -- x
    pn = last parts
    -- 14
    i = sum $ zipWith (*) parts' [1 ..]
prettyName n = n

-- makes generated vars more readable in the whole expression
prettyVar :: Expr -> Expr
prettyVar (Var v rs) = Var (prettyName v) (map prettyVar rs)
prettyVar (Ctr n args)   = Ctr n (map prettyVar args)
prettyVar (FCall n args) = FCall n (map prettyVar args)
prettyVar (GCall n args) = GCall n (map prettyVar args)
prettyVar (TestEq (a1, a2) (e1, e2)) = TestEq (prettyVar a1, prettyVar a2) (prettyVar e1, prettyVar e2)
prettyVar e = e
