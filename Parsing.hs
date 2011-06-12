module Parsing where
    
import Char
import Data

instance Read Expr where
    readsPrec _ s = readExpr s

instance Read Program where
    readsPrec _ s = readProgram s

readArgs :: ReadS [Expr]
readArgs s0 = [(args, s2)] where
    [("(", s1)] = lex s0
    [(args, s2)] = readArgs' s1
    
-- TODO: generalize readArgs and readVar
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

----    
readVars' :: ReadS [Name]
readVars' s = case lex s of
    [(")", s1)] -> [([], s1)]
    [(",", s1)] -> [(arg:args, s3)] where
        [(arg, s2)] = lex s1
        [(args, s3)] = readVars' s2
    [(_, _)] -> [(arg:args, s3)] where
        [(arg, s2)] = lex s
        [(args, s3)] = readVars' s2
---
readExpr :: ReadS Expr
readExpr s = case lex s of 
    [("if", s1)] -> [(TestEq (a1, a2) (e1, e2), s2)] where
        [([a1, a2, e1, e2], s2)] = readArgs s1
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

readProgram :: ReadS Program
readProgram s = [readP1 (Program [] []) s] where

readP1 p@(Program fs gs) s = oneOf (readFDef s) (readGDef s) where
    oneOf [(f, s1)] _ = readP1 (Program (fs++[f]) gs) s1
    oneOf _ [(g, s1)] = readP1 (Program fs (gs++[g])) s1
    oneOf _ _ = (p, s)