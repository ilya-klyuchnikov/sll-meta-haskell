module Examples where

import System.Timeout

import Data
import DataUtil
import DataIO
import Driving
import GenVar
import NeighborhoodAnalysis
import URA
import Interpreter

-- it is safe reading - we read only once
import System.IO.Unsafe

loadProgram :: FilePath -> Program
loadProgram path = read (unsafePerformIO (readFile path))
progString = loadProgram "Examples/string.sll"
progTree = loadProgram "Examples/tree.sll"
progList = loadProgram "Examples/list.sll"

-- helper function to run URA demonstration
-- it is a bit tricky in order to indicated not terminated runs
sampleURA :: String -> Program -> String -> String -> IO ()
sampleURA comment prog inputConfText resultText = do
    putStrLn ("\n===================\n"++ comment ++ "\nURA task:")
    putStrLn ("\t" ++ (show input) ++ " -> " ++ (show output))
    putStrLn "answer:"
    putStrLn $ withDelim "\n" $ map (("\t" ++)) $ map prettySub $ take 10 result
    putStrLn note
    putStrLn note1
    where
        input = (read inputConfText) :: Expr
        output = (read resultText) :: Expr
        result = ura (perfectDriveMachine prog) input output
        many = length (take 101 result) > 100
        note = if many then "\t> 100 (infinitely many?) SOLUTIONS" else ""
        note1 = "\t" ++ (show $ length result) ++ " results"

sampleNan :: String -> Program -> String -> String -> IO ()
sampleNan comment prog conf center = do
    putStrLn ("\n===================\n" ++ comment ++ "\nNAN task:")
    putStrLn ("\t" ++ (show inputConf) ++ " <> " ++ (show inputData))
    putStrLn "answer:"
    putStrLn $ ("\t" ++) $ show $ prettySub result
    where
        inputConf = (read conf) :: Expr
        inputData = (read center) :: Expr
        result = nan (perfectDriveMachine prog) inputConf inputData

sampleURA01 = sampleURA
        "sampleURA01 - which string IS a substring of `AB`?"
        progString
        "fMatch(x, Cons('A', Cons('B', Nil())))"
        "'T'"

sampleURA02 = sampleURA
        "sampleURA02 - which string IS NOT a substring of `AB`?"
        progString
        "fMatch(x, Cons('A', Cons('B', Nil())))"
        "'F'"

sampleURA03 = sampleURA
        "sampleURA03 - which tree can be flatten to `A`? (no termination here)"
        progTree
        "gListEq(Cons('A', Nil()), gFlatten(x))"
        "'T'"

sampleURA04 = sampleURA
        "sampleURA04 - which tree can be flatten to `ABCDEFG`? (no termination here)"
        progTree
        "gListEq(Cons('a', Cons('b', Cons('c', Cons('d', Cons('e', Cons('f', Cons('g', Nil()))))))), gFlatten(t))"
        "'T'"

sampleURA05 = sampleURA
        "sampleURA05 - which tree can be flatten to `ABC`?"
        progTree
        "gListEq(Cons('a', Cons('b', Cons('c', Nil()))), gFlatten(t))"
        "'T'"

sampleURA06 = sampleURA
        "sampleURA06 - all trees of size 1"
        progTree
        "gEq(S(Z()), gSize(t))"
        "'T'"

sampleURA07 = sampleURA
        "sampleURA07 - all trees of size 2"
        progTree
        "gEq(S(S(Z())), gSize(t))"
        "'T'"

sampleURA08 = sampleURA
        "sampleURA08 - all trees of size 5"
        progTree
        "gEq(S(S(S(S(S(Z()))))), gSize(t))"
        "'T'"


sampleURA09 = sampleURA
        "sampleURA09 - all trees whose flattened representations are the same (infinitely many solutions)"
        progTree
        "gListEq(gFlatten(tl), gFlatten(tr))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"
-- strings that equal to "B" after replacing all 'A' -> 'B'
sampleURA10 = sampleURA
        "sampleURA10 - which string after replacing all `A` to `B` is `B`?"
        progString
        "gStrEq(Cons('B', Nil()), ga2b(s))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"
-- strings that equal to "BBB" after replacing all 'A' -> 'B'
sampleURA11 = sampleURA
        "sampleURA11 - which string after replacing all `A` to `B` is `BBB`?"
        progString
        "gStrEq(Cons('B', Cons('B', Cons('B', Nil()))), ga2b(Cons(c, Cons(c, Cons(c, Nil())))))"
        "'T'"

-- (x, y) such that x IS NOT a substring of y
sampleURA12 = sampleURA
        "sampleURA12 - x IS NOT a substring of y"
        progString
        "fMatch(x, y)"
        "'F'"

-- (x, y) such that x IS a substring of y
sampleURA13 = sampleURA
        "sampleURA13 - x IS a substring of y"
        progString
        "fMatch(x, y)"
        "'T'"

sampleURA14 = sampleURA
        "sampleURA14 - "
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(t))"
        "'T'"

-- but we try to mitigate it step-by-step
-- we abstract let ls = flatten(t)
-- this gives an answer ls -> Cons('a', Nil())
sampleURA15 = sampleURA
        "sampleURA15"
        progTree
        "gListEq(Cons('a', Nil()), ls)"
        "'T'"

-- which tree can be flatten to "A"?
-- no termination here
sampleURA16 = sampleURA
        "sampleURA16"
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(t))"
        "'T'"

-- next, we looking into definition of flatten
-- gFlatten(Leaf(a)) = Cons(a, Nil());
-- gFlatten(Node(lt, s, rt)) = gAppend(gFlatten(lt), Cons(s, gFlatten(rt)));
-- and consider 2 variants

-- this variant gives a -> 'a'
sampleURA17 = sampleURA
        "sampleURA17"
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(Leaf(a)))"
        "'T'"

-- this variant haltls
sampleURA18 = sampleURA
        "sampleURA18"
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(Node(lt, s, rt)))"
        "'T'"

-- we abstract
-- l1 = gFlatten(lt)
-- l2 = gFlatten(rt)
-- and get answers: l1 = Nil, l2 = Nil
sampleURA19 = sampleURA
        "sampleURA19"
        progTree
        "gListEq(Cons('a', Nil()), gAppend(l1, Cons(s, l2)))"
        "'T'"

-- continue. This halts
sampleURA20 = sampleURA
        "sampleURA20"
        progTree
        "gListEq(Nil(), gFlatten(lt))"
        "'T'"

-- again, we consider two variants
-- 1) empty answer
sampleURA21 = sampleURA
        "sampleURA21"
        progTree
        "gListEq(Nil(), gFlatten(Leaf(x)))"
        "'T'"

-- we abstract
-- l1 = gFlatten(lt)
-- l2 = gFlatten(rt)
-- empty answer
sampleURA22 = sampleURA
        "sampleURA22"
        progTree
        "gListEq(Nil(), gAppend(l1, Cons(s, l2)))"
        "'T'"

sampleURA23 = sampleURA
        "sampleURA23"
        progList
        "gEqNat(x, S(Z()))"
        "T()"

sampleURA24 = sampleURA
        "sampleURA24"
        progList
        "gContains(x, 'a')"
        "T()"

-- this doesn't terminate -
-- but this is non-flat expression!!
sampleURA25 = sampleURA
        "sampleURA25"
        progList
        "gEqNat(fSize(x), S(Z()))"
        "T()"

-- The simplest non-termination of URA for flat function
-- is there a simple way to analyze it??
sampleURA26 = sampleURA
        "sampleURA26"
        progList
        "gT(x, T())"
        "F()"

sampleNAN01 = sampleNan
        "sampleNAN01"
        progString
        "fEq(x, y)"
        "fEq('A', 'A')"

sampleNAN02 = sampleNan
        "sampleNAN02"
        progString
        "fEq(x, y)"
        "fEq('A', 'B')"

sampleNAN03 = sampleNan
        "sampleNAN03"
        progString
        "fMatch(x, y)"
        "fMatch(Cons('A', Nil()), Cons('A', Cons('A', Nil())))"

sampleNAN04 = sampleNan
        "sampleNAN04"
        progString
        "fMatch(x, y)"
        "fMatch(Cons('A', Nil()), Cons('B', Cons('A', Nil())))"

-- compare sampleNan5 and sampleNan6
sampleNAN05 = sampleNan
        "sampleNAN05 - NAN for non-flat expression"
        progString
        "P(gStrEq(x, Cons('B', Nil())))"
        "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())))"

sampleNAN06 = sampleNan
        "sampleNAN06 - NAN for non-flat expression"
        progString
        "P(gStrEq(x, Cons('B', Nil())), gStrEq(x, Cons('C', Nil())))"
        "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())), gStrEq(Cons('A', Nil()), Cons('C', Nil())))"

sampleNAN07 = sampleNan
        "sampleNAN07 - NAN for non-flat expression"
        progString
        "P(x, fEq(x, y))"
        "P('A', fEq('A', 'B'))"

-- smart runner
-- if some sample doesn't terminate in timeout, it will add ..............
-- to the output
run :: IO () -> IO ()
run sample = do
    res <- (timeout 2000000 sample)
    case res of
        Nothing -> do
            putStrLn "\n............"
            putStrLn "............"
            return ()
        Just _ -> do
            return ()

main :: IO ()
main = do
    run sampleURA01
    run sampleURA02
    run sampleURA03
    run sampleURA04
    run sampleURA05
    run sampleURA06
    run sampleURA07
    run sampleURA08
    run sampleURA09
    run sampleURA10
    run sampleURA11
    run sampleURA12
    run sampleURA13
    run sampleURA14
    run sampleURA15
    run sampleURA16
    run sampleURA17
    run sampleURA18
    run sampleURA19
    run sampleURA20
    run sampleURA21
    run sampleURA22
    run sampleURA23
    run sampleURA24
    run sampleURA25
    run sampleNAN01
    run sampleNAN02
    run sampleNAN03
    run sampleNAN04
    run sampleNAN05
    run sampleNAN06
    run sampleNAN07
    return ()
