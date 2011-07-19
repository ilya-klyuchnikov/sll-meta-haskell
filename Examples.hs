module Examples where

import Data
import DataUtil
import DataIO
import Driving
import Folding

import NeighborhoodAnalysis
import URA

progString :: Program
progString = read
    " fMatch(p, s) = gM(p, s, p, s);\
    \ gM(Nil(), ss, op, os) = 'T';\
    \ gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);\
    \ gX(Nil(), p, pp,  op, os) = 'F';\
    \ gX(Cons(s, ss), p, pp,  op, os) = if(s, p, gM(pp, ss, op, os), gN(os, op));\
    \ gN(Nil(), op) = False(); \
    \ gN(Cons(s, ss), op) = gM(op, ss, op, ss); \
    \ fEq(x, y) = if(x, y, 'T', 'F'); \
    
    \ ga2b(Nil()) = Nil(); \
    \ ga2b(Cons(x, xs)) = if(x, 'A', Cons('B', ga2b(xs)), Cons(x, ga2b(xs))); \
    
    \ gStrEq(Cons(a1, s1), s2) = gStrEq1(s2, a1, s1); \ 
    \ gStrEq(Nil(), s2) = gStrEq2(s2); \ 
    \ gStrEq1(Cons(a2, s2), a1, s1) = if(a1, a2, gStrEq(s1, s2), 'F'); \
    \ gStrEq1(Nil(), a1, s1) = 'F'; \
    \ gStrEq2(Nil()) = 'T'; \
    \ gStrEq2(Cons(x, xs)) = 'F'; "

progTree :: Program
progTree = read
    " gFlatten(Leaf(a)) = Cons(a, Nil()); \
    \ gFlatten(Node(lTree, s, rTree)) = gAppend(gFlatten(lTree), Cons(s, gFlatten(rTree))); \
    
    \ gAppend(Nil(), ys) = ys; \
    \ gAppend(Cons(x, xs), ys) = Cons(x, gAppend(xs, ys)); \
    
    \ gAdd(Z(), y) = y;\
    \ gAdd(S(x), y) = S(gAdd(x, y));\
    
    \ gEq(Z(), y) = gEqZ(y); \
    \ gEq(S(x), y) = gEqS(y, x); \
    \ gEqZ(Z()) = 'T'; \
    \ gEqZ(S(x)) = 'F'; \
    \ gEqS(Z(), x) = 'F'; \
    \ gEqS(S(y), x) = gEq(x, y); \
    
    \ gSize(Leaf(a)) = S(Z()); \
    \ gSize(Node(lTree, s, rTree)) = S(gAdd(gSize(lTree), gSize(rTree))); \
    
    \ gListEq(Cons(x, xs), ys) = gListEq1(ys, x, xs); \ 
    \ gListEq(Nil(), ys) = gListEq2(ys); \ 
    \ gListEq1(Cons(y, ys), x, xs) = if(x, y, gListEq(xs, ys), 'F'); \
    \ gListEq1(Nil(), x, xs) = 'F'; \
    \ gListEq2(Nil()) = 'T'; \
    \ gListEq2(Cons(y, ys)) = 'F';"

sampleURA prog inputConfText resultText = do
    putStrLn "task:"
    putStrLn ("\t" ++ (show input) ++ " -> " ++ (show output)) 
    putStrLn "answer:"
    putStrLn $ withDelim "\n" $ map (("\t" ++) . show) $ map prettySub result 
    where
        input = (read inputConfText) :: Expr
        output = (read resultText) :: Expr
        result = ura (perfectDriveMachine prog) input output

sampleNan prog conf center = do
    putStrLn "task:"
    putStrLn ("\t" ++ (show inputConf) ++ " <> " ++ (show inputData)) 
    putStrLn "answer:"
    putStrLn $ ("\t" ++) $ show $ prettySub result 
    where
        inputConf = (read conf) :: Expr
        inputData = (read center) :: Expr
        result = nan (perfectDriveMachine prog) inputConf inputData 

prettySub = map (\(x, y) -> (x, prettyVar y))

-- which string IS a substring of "AB"?
sampleURA1 = sampleURA 
        progString 
        "fMatch(x, Cons('A', Cons('B', Nil())))"
        "'T'"

-- which string IS NOT a substring of "AB"?
sampleURA2 = sampleURA 
        progString 
        "fMatch(x, Cons('A', Cons('B', Nil())))"
        "'F'" 
                
-- which tree can be flatten to "ABCDEFG"?
sampleURA3 = sampleURA 
        progTree 
        "gListEq(Cons('a', Cons('b', Cons('c', Cons('d', Cons('e', Cons('f', Cons('g', Nil()))))))), gFlatten(t))"
        "'T'" 

-- which tree can be flatten to "ABC"?
sampleURA3' = sampleURA 
        progTree 
        "gListEq(Cons('a', Cons('b', Cons('c', Nil()))), gFlatten(t))"
        "'T'" 

-- all trees of size 1
sampleURA4 = sampleURA
        progTree
        "gEq(S(Z()), gSize(t))"
        "'T'"
                
-- all trees of size 2
sampleURA5 = sampleURA 
        progTree
        "gEq(S(S(Z())), gSize(t))"
        "'T'"
                
-- all trees of size 5
sampleURA6 = sampleURA
        progTree
        "gEq(S(S(S(S(S(Z()))))), gSize(t))"
        "'T'"
   
-- all trees whose flattened representations are the same
sampleURA7 = sampleURA
        progTree
        "gListEq(gFlatten(tl), gFlatten(tr))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"
sampleURA8 = sampleURA
        progString
        "gStrEq(Cons('B', Nil()), ga2b(s))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"        
sampleURA9 = sampleURA
        progString
        "gStrEq(Cons('B', Cons('B', Cons('B', Nil()))), ga2b(Cons(c, Cons(c, Cons(c, Nil())))))"
        "'T'"
        
sampleURA10 = sampleURA
        progString
        "fMatch(x, y)"
        "'F'"

sampleURA11 = sampleURA
        progString
        "fMatch(x, y)"
        "'T'"

sampleNan1 = sampleNan
        progString
        "fEq(x, y)"
        "fEq('A', 'A')"

sampleNan2 = sampleNan
        progString
        "fEq(x, y)"
        "fEq('A', 'B')"

sampleNan3 = sampleNan
        progString
        "fMatch(x, y)"
        "fMatch(Cons('A', Nil()), Cons('A', Cons('A', Nil())))"
        
sampleNan4 = sampleNan 
        progString
        "fMatch(x, y)"
        "fMatch(Cons('A', Nil()), Cons('B', Cons('A', Nil())))"

-- compare sampleNan5 and sampleNan6
sampleNan5 = sampleNan 
        progString
        "P(gStrEq(x, Cons('B', Nil())))"
        "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())))"

sampleNan6 = sampleNan 
        progString
        "P(gStrEq(x, Cons('B', Nil())), gStrEq(x, Cons('C', Nil())))"
        "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())), gStrEq(Cons('A', Nil()), Cons('C', Nil())))"
        
sampleNan7 = sampleNan
        progString
        "P(x, fEq(x, y))"
        "P('A', fEq('A', 'B'))"

sampleSC =
    putStrLn $ printTree $ foldTree $ buildConfTree (perfectDriveMachine progString) 
        (read "fMatch(Cons('A', Cons('A', Nil())), s)")