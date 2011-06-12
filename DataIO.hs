module DataIO where

import Data
import DataUtil
import Maybe
import Char
import List

import Data.List

printTree t = unlines $ take 1000 $ pprintTree "" "" t

pprintTree :: String -> String -> Graph Conf -> [String]
pprintTree indent msg (Node expr next) = make next where
    make (EFold _ ren) = (indent ++ msg) : [indent ++ "|__" ++  (show expr) ++ "__|" ++ (show ren)]
    make (ETransient _ t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
    make (EDecompose comp ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
    make (EVariants cs) = 
        (indent ++ msg) :  (indent ++ "|__" ++  show expr) : (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ show x) t) cs))
pprintTree indent msg (Leaf expr) = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]
    

instance Show Expr where
    show (Var n []) = n
    show (Var n rs) = n ++ "<" ++ (withDelim ", " (map (("!=" ++). show) rs)) ++ ">"
    show (Ctr n es) = n ++ "(" ++ (withDelim ", " (map show es)) ++ ")"
    show (FCall n es) = (fn n) ++ "(" ++ (withDelim ", " (map show es)) ++ ")"
    show (GCall n es) = (fn n) ++ "(" ++ (withDelim ", " (map show es)) ++ ")"
    show (Atom a) = show a
    show (TestEq (a1, a2) (e1, e2)) = "if(" ++ (show a1) ++ " == " ++ (show a2) ++", " ++ (show e1) ++ ", " ++ (show e2) ++ ")"

fn :: String -> String  
fn (_:s:ss) = (toLower s) : ss

instance Show FDef where
    show (FDef n args body) = (fn n) ++ "(" ++ withDelim ", " args ++ ") = " ++ (show body) ++ ";"

instance Show GDef where
    show (GDef n p args body) = (fn n) ++ "(" ++ withDelim ", " (show p:args) ++ ") = " ++ (show body) ++ ";"

instance Show Pat where
    show (Pat cn vs) = cn ++ "(" ++ withDelim ", " vs ++ ")"
    
instance Show a => Show (Contraction a) where
    show (Contraction n p) = n ++ " == " ++ (show p)
    
instance Show Program where
    show (Program fs gs) = withDelim "\n" $ (map show fs) ++ (map show gs)
    
instance Show a => Show (Step a) where
    show (Transient _ a) = "=> " ++ (show a)
    show (Variants vs) = withDelim "\n" $ map (\(c, e) -> (show c) ++ " => " ++ (show e)) vs 
    show (Stop _) = "!"
    show (Decompose _ ds) = "DEC " ++ (show ds)

-- Latex
pprintLTree :: Graph Conf -> String
pprintLTree (Leaf expr) = "node[conf]{" ++ (show expr) ++ "}"
pprintLTree (Node expr next) = make next where
    make (EFold _ _) = "node[conf]{" ++ (show expr) ++ "}"
    make (ETransient _ t) = "node[conf]{" ++ (show expr) ++ "}\nchild[->]{" ++ (pprintLTree t) ++ "}"
    make (EDecompose _ ts) = "node[conf]{" ++ (show expr) ++ "}" ++ 
        (concat (map (\t -> "\nchild[->]{" ++ (pprintLTree t) ++ "}") ts))
    make (EVariants [(x1, t1), (x2, t2)]) = 
        "node[conf]{" ++ (show expr) ++ "}" ++ 
            ("\nchild[->]{" ++ (pprintLTree t1) ++ "\nedge from parent node[left,label,xshift=-5mm]{" ++ (show x1) ++ "}}") ++
            ("\nchild[->]{" ++ (pprintLTree t2) ++ "\nedge from parent node[right,label,xshift=5mm]{" ++ (show x2) ++ "}}")

withDelim :: [a] -> [[a]] -> [a]
withDelim xs xss = concat (intersperse xs xss)