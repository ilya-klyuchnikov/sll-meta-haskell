module Demonstration where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Folding
import Data.List
import Data.Maybe
import NeighborhoodAnalysis
import URA

prog1 :: Program
prog1 = read
    " gAdd(Z(), y) = y;\
    \ gAdd(S(x), y) = S(gAdd(x, y));\
    \ gMult(Z(), y) = Z();\
    \ gMult(S(x), y) = gAdd(y, gMult(x, y));\ 
    \ fSqr(x) = gMult(x, x); \
    \ gEven(Z()) = True();\
    \ gEven(S(x)) = gOdd(x);\
    \ gOdd(Z()) = False();\
    \ gOdd(S(x)) = gEven(x);\
    \ gAdd1(Z(), y) = y; \
    \ gAdd1(S(x), y) = gAdd1(x, S(y)); \
    
    \ gLt(Z(), y)  = gLt1(y);\
    \ gLt(S(x), y) = gLt2(y, x);\
    
    \ gLt1(Z()) = False();\
    \ gLt1(S(x)) = True();\
    
    \ gLt2(Z(), x) = False();\
    \ gLt2(S(y), x) = gLt(x, y);\
    
    \ fTest(x, y, z) = P(gLt(x, z), gLt(y, z));"

prog2 :: Program
prog2 = read
    " gEqSymb(A(), y) = gEqA(y);\
    \ gEqSymb(B(), y) = gEqB(y);\
    \ gEqA(A()) = True();  gEqA(B()) = False();\
    \ gEqB(A()) = False(); gEqB(B()) = True();\
    \ gIf(True(), x, y) = x;\
    \ gIf(False(), x, y) = y;\
    \ fMatch(p, s) = gM(p, s, p, s);\
    \ gM(Nil(), ss, op, os) = True();\
    \ gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);\
    \ gX(Nil(), p, pp,  op, os) = False();\
    \ gX(Cons(s, ss), p, pp,  op, os) = gIf(gEqSymb(p, s), gM(pp, ss, op, os), gN(os, op));\
    \ gN(Nil(), op) = False(); \
    \ gN(Cons(s, ss), op) = gM(op, ss, op, ss);"
    
prog3 :: Program
prog3 = read
    " gAdd(Z(), y) = y;\
    \ gAdd(S(x), y) = S(gAdd(x, y));\
    \ gDouble(Z()) = Z(); \
    \ gDouble(S(x)) = S(S(gDouble(x))); \
    \ gHalf(Z()) = Z(); \
    \ gHalf(S(x)) = gHalf1(x); \ 
    \ gHalf1(Z()) = Z(); \
    \ gHalf1(S(x)) = S(gHalf(x)); \
    \ gEq(Z(), y) = gEqZ(y); \
    \ gEq(S(x), y) = gEqS(y, x); \
    \ gEqZ(Z()) = True(); \
    \ gEqZ(S(x)) = False(); \
    \ gEqS(Z(), x) = False(); \
    \ gEqS(S(y), x) = gEq(x, y);"
    
    
prog4 :: Program
prog4 = read
    " fInf() = S(fInf()); \
    \ fB(x) = fB(S(x));"
    
progMatch :: Program
progMatch = read
    " fMatch(p, s) = gM(p, s, p, s);\
    \ gM(Nil(), ss, op, os) = 'T';\
    \ gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);\
    \ gX(Nil(), p, pp,  op, os) = 'F';\
    \ gX(Cons(s, ss), p, pp,  op, os) = if(s, p, gM(pp, ss, op, os), gN(os, op));\
    \ gN(Nil(), op) = False(); \
    \ gN(Cons(s, ss), op) = gM(op, ss, op, ss); \
    \ fEq(x, y) = if(x, y, 'T', 'F'); \
    
    \ gStrEq(Cons(a1, s1), s2) = gStrEq1(s2, a1, s1); \ 
    \ gStrEq(Nil(), s2) = gStrEq2(s2); \ 
    \ gStrEq1(Cons(a2, s2), a1, s1) = if(a1, a2, gStrEq(s1, s2), 'F'); \
    \ gStrEq1(Nil(), a1, s1) = 'F'; \
    \ gStrEq2(Nil()) = 'T'; \
    \ gStrEq2(Cons(x, xs)) = 'F'; "

task1 :: Conf
task1 = read ("fMatch(Cons('A', Cons('A', Cons('B', Nil()))), Nil())")

task2 :: Conf
task2 = read ("fMatch(Cons('A', Cons('A', Cons('B', Nil()))), Cons('A', Cons('A', Cons('C', Nil()))))")

task3 :: Conf
task3 = read ("fMatch(Cons('A', Cons('A', Cons('B', Nil()))), Cons('A', Cons('A', Cons('B', Nil()))))")


demo01 = eval progMatch task1
demo02 = eval progMatch task2
demo03 = eval progMatch task3

-- trying eval undefined expression
demo07 =
    eval prog1 $ read "fSqr(S(S(x)))"

-- "eval" infinite number   
demo09 =
    eval prog4 $ read "fInf()"

--  driving (variants)
demo10 =
    (driveMachine prog1) (read "gOdd(gAdd(x, gMult(x, S(x))))")

demo10a =
    (perfectDriveMachine prog1) (read "gOdd(gAdd(x, gMult(x, S(x))))")

-- driving (transient step) 
demo11 = 
    (driveMachine prog1) (read "gOdd(S(gAdd(v1, gMult(x, S(x)))))")

demo11a = 
    (perfectDriveMachine prog1) (read "gOdd(S(gAdd(v1, gMult(x, S(x)))))")
    
-- building infinite tree
demo12 =
    putStrLn $ printTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")

-- using intTree (infinite tree) to run task 
demo13 =
    intTree (buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(Z()))")]
    
-- using intTree (folded finite graph) to run task
demo13a = 
    intTree (foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(Z()))")]

-- using intTree (infinite tree) to run task
demo14 =
    intTree (buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(S(Z())))")]

-- successful folding
demo15 =
    putStrLn $ printTree $ foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")
    
-- successful folding (tex)
demo15a =
    putStrLn $ pprintLTree $ foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")

-- an example of "not foldable" tree
demo16 =
    putStrLn $ printTree $ foldTree $ buildTree (driveMachine prog1) (read "gAdd1(x, y)")

demo20 =
    putStrLn $ printTree $ buildTree (perfectDriveMachine progMatch) 
        (read "fMatch(Cons(a, Cons(b, Nil())), Cons(b, Cons(d, Nil())))")

demo21 =
    putStrLn $ printTree $ buildTree (perfectDriveMachine progMatch) 
        (read "fMatch(Cons(a, Cons(b, Nil())), Cons('A', Cons('B', Nil())))")

demo22 =
    putStrLn $ printTree $ buildTree (perfectDriveMachine progMatch) 
        (read "fMatch(Cons(x, Cons('A', Nil())), Cons('A', Cons('B', Nil())))")

demo23 =
    putStrLn $ printTree $ buildTree (perfectDriveMachine progMatch) 
        (read "fMatch(x, Cons('A', Cons('B', Nil())))")


main = do
    putStrLn "\ndemo06"
    -- error
    --putStrLn (show demo06)
    
    putStrLn "\ndemo07"
    -- error
    --putStrLn (show demo07)
    
    putStrLn "\ndemo08"
    -- bottom
    --putStrLn (show demo08)
    
    putStrLn "\ndemo09"
    -- infinite
    --putStrLn (show demo09)
    
    putStrLn "\ndemo10"
    putStrLn (show demo10)
    
    putStrLn "\ndemo11"
    putStrLn (show demo11)
    
    putStrLn "\ndemo12"
    demo12
    
    putStrLn "\ndemo13"
    putStrLn (show demo13)
    
    putStrLn "\ndemo14"
    putStrLn (show demo14)
    
    putStrLn "\ndemo15"
    demo15
    
    putStrLn "\ndemo16"
    demo16

v1 = Var "v1" [Var "v2" []]
v2 = Atom 'A'
sub =  [("v2", v2)]

