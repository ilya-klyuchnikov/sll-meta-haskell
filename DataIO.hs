module DataIO where

import Data
import DataUtil
import Data.Maybe
import Data.Char
import Data.List
import GenVar

-- pretty printing of expressions and programs,
-- parsing of expressions and programs
-- pretty printing of trees

withDelim :: [a] -> [[a]] -> [a]
withDelim xs xss = concat (intersperse xs xss)

-- SLL pretty printing
instance Show Expr where
    show (Var n []) = prettyName n
    show (Var n rs) = (prettyName n) ++ "<" ++ (withDelim ", " (map (("!=" ++). show) rs)) ++ ">"
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

instance Show Program where
    show (Program fs gs) = withDelim "\n" $ (map show fs) ++ (map show gs)

-- SLL parsing
instance Read Expr where
    readsPrec _ s = readExpr s

instance Read Program where
    readsPrec _ s = readProgram s

readArgs :: ReadS [Expr]
readArgs s0 = [(args, s2)] where
    [("(", s1)] = lex s0
    [(args, s2)] = readArgs' s1

readArgs' :: ReadS [Expr]
readArgs' s = case lex s of
    [(")", s1)] -> [([], s1)]
    [(",", s1)] -> [(arg:args, s3)] where
        [(arg, s2)] = readExpr s1
        [(args, s3)] = readArgs' s2
    [(_, _)] -> [(arg:args, s3)] where
        [(arg, s2)] = readExpr s
        [(args, s3)] = readArgs' s2

readVars :: ReadS [Name]
readVars s0 = [(args, s2)] where
    [("(", s1)] = lex s0
    [(args, s2)] = readVars' s1

readVars' :: ReadS [Name]
readVars' s = case lex s of
    [(")", s1)] -> [([], s1)]
    [(",", s1)] -> [(arg:args, s3)] where
        [(arg, s2)] = lex s1
        [(args, s3)] = readVars' s2
    [(_, _)] -> [(arg:args, s3)] where
        [(arg, s2)] = lex s
        [(args, s3)] = readVars' s2

readExpr :: ReadS Expr
readExpr s = case lex s of
    [("if", s1)] -> [(TestEq (a1, a2) (e1, e2), s8)] where
        [(a1,     s2)] = readExpr s1
        [("=",    s3)] = lex s2
        [(a2,     s4)] = readExpr s3
        [("then", s5)] = lex s4
        [(e1,     s6)] = readExpr s5
        [("else", s7)] = lex s6
        [(e2,     s8)] = readExpr s7
    [(n@('g':_), s1)] -> [(GCall n args, s2)] where
        [(args, s2)] = readArgs s1
    [(n@('f':_), s1)] -> [(FCall n args, s2)] where
        [(args, s2)] = readArgs s1
    [(n@(x:_), s1)] | isUpper x -> [(Ctr n args, s2)] where
        [(args, s2)] = readArgs s1
    [(['\'', a, '\''], s1)] -> [(Atom a, s1)]
    [(n@(x:_), s1)] | isLower x -> [(Var n [], s1)]

readPat :: ReadS Pat
readPat i = do
    (n, s) <- lex i
    (vars, s2) <- readVars s
    return (Pat n vars, s2)

readFDef :: ReadS FDef
readFDef i = do
    (n@('f':_), s) <- lex i
    (vars, s1) <- readVars s
    ("=", s2) <- lex s1
    (body, s3) <- readExpr s2
    (";", s4) <- lex s3
    return (FDef n vars body, s4)

readGDef :: ReadS GDef
readGDef i = do
    (n@('g':_), s) <- lex i
    ("(", s1) <- lex s
    (p, s2) <- readPat s1
    (vs, s3) <- readVars' s2
    ("=", s4) <- lex s3
    (body, s5) <- readExpr s4
    (";", s6) <- lex s5
    return (GDef n p vs body, s6)

readComment :: ReadS ()
readComment i = readComment1 (dropWhile isSpace i)

readComment1 ('{' : '-' : s) = readComment2 s
readComment1 _               = []
readComment2 ('-' : '}' : s) = [((), s)]
readComment2 (_ : s)         = readComment2 s
readComment2 []              = []

readProgram :: ReadS Program
readProgram s = [readProgram' (Program [] []) s]

readProgram' p@(Program fs gs) s = oneOf (readComment s) (readFDef s) (readGDef s) where
    oneOf [(_, s1)] _ _ = readProgram' (Program  fs       gs) s1
    oneOf _ [(f, s1)] _ = readProgram' (Program (fs++[f]) gs) s1
    oneOf _ _ [(g, s1)] = readProgram' (Program fs (gs++[g])) s1
    oneOf _ _ _ = (p, s)

-- pretty printing of steps
instance Show a => Show (Step a) where
    show (Transient _ a) = "=> " ++ (show a)
    show (Variants vs) = withDelim "\n" $ map (\(c, e) -> (show c) ++ " => " ++ (show e)) vs
    show (Stop _) = "!"
    show (Decompose _ ds) = "DEC " ++ (show ds)

-- pretty printing of trees
printTree t = unlines $ take 90 $ pprintTree "" "" t

pprintTree :: String -> String -> Tree Expr -> [String]
pprintTree indent msg (Node expr next) = make next where
    make (ETransient _ t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
    make (EDecompose comp ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
    make (EVariants cs) =
        (indent ++ msg) :  (indent ++ "|__" ++  show expr) :
            --(concat (map (\([(v, ptr)], t) -> pprintTree (indent ++ " ") ("?" ++ (prettyName v) ++ " -> " ++ (show ptr)) t) cs))
            (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ (prettySub x)) t) cs))
pprintTree indent msg (Leaf expr) = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]

prettySub [] = ""
prettySub ((x, ptr):[]) = (prettyName x) ++ " -> " ++ (show ptr)
prettySub ((x, ptr):bs) = (prettyName x) ++ " -> " ++ (show ptr) ++ ", " ++ (prettySub bs)
