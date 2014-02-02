module Examples where

import System.Timeout

import Data
import DataUtil
import DataIO
import Driving
import GenVar
import NeighborhoodAnalysis
import URA

-- it is safe reading - we read only once
import System.IO.Unsafe

loadProgram :: FilePath -> Program
loadProgram path = read (unsafePerformIO (readFile path))

progString :: Program
progString = loadProgram "Examples/string.sll"

progTree :: Program
progTree = loadProgram "Examples/tree.sll"

progList :: Program
progList = loadProgram "Examples/list.sll"

-- helper function to run URA demonstration
sampleURA :: Program -> String -> String -> IO ()
sampleURA prog inputConfText resultText = do
    putStrLn "===================\nURA task:"
    putStrLn ("\t" ++ (show input) ++ " -> " ++ (show output))
    putStrLn "answer:"
    putStrLn $ withDelim "\n" $ map (("\t" ++) . show) $ map prettySub result
    where
        input = (read inputConfText) :: Expr
        output = (read resultText) :: Expr
        result = ura (perfectDriveMachine prog) input output

sampleNan :: Program -> String -> String -> IO ()
sampleNan prog conf center = do
    putStrLn "===================\nNAN task:"
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
-- no termination here
sampleURA3 = sampleURA
        progTree
        "gListEq(Cons('a', Cons('b', Cons('c', Cons('d', Cons('e', Cons('f', Cons('g', Nil()))))))), gFlatten(t))"
        "'T'"

-- which tree can be flatten to "ABC"?
-- no termination here
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
-- no termination
sampleURA7 = sampleURA
        progTree
        "gListEq(gFlatten(tl), gFlatten(tr))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"
-- strings that equal to "B" after replacing all 'A' -> 'B'
sampleURA8 = sampleURA
        progString
        "gStrEq(Cons('B', Nil()), ga2b(s))"
        "'T'"

-- see example in
-- "Faster Answers and Improved Termination in Inverse Computation of Non-Flat Languages"
-- strings that equal to "BBB" after replacing all 'A' -> 'B'
sampleURA9 = sampleURA
        progString
        "gStrEq(Cons('B', Cons('B', Cons('B', Nil()))), ga2b(Cons(c, Cons(c, Cons(c, Nil())))))"
        "'T'"

-- (x, y) such that x IS NOT a substring of y
sampleURA10 = sampleURA
        progString
        "fMatch(x, y)"
        "'F'"

-- (x, y) such that x IS a substring of y
sampleURA11 = sampleURA
        progString
        "fMatch(x, y)"
        "'T'"

sampleURA12 = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(t))"
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

-- which tree can be flatten to "A"?
-- no termination here
sampleURA20 = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(t))"
        "'T'"

-- but we try to mitigate it step-by-step
-- we abstract let ls = flatten(t)
-- this gives an answer ls -> Cons('a', Nil())
sampleURA13 = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), ls)"
        "'T'"

-- next, we looking into definition of flatten
-- gFlatten(Leaf(a)) = Cons(a, Nil());
-- gFlatten(Node(lt, s, rt)) = gAppend(gFlatten(lt), Cons(s, gFlatten(rt)));
-- and consider 2 variants

-- this variant gives a -> 'a'
sampleURA21a = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(Leaf(a)))"
        "'T'"

-- this variant haltls
sampleURA21b = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), gFlatten(Node(lt, s, rt)))"
        "'T'"

-- we abstract
-- l1 = gFlatten(lt)
-- l2 = gFlatten(rt)
-- and get answers: l1 = Nil, l2 = Nil
sampleURA21c = sampleURA
        progTree
        "gListEq(Cons('a', Nil()), gAppend(l1, Cons(s, l2)))"
        "'T'"

-- continue. This halts
sampleURA22a = sampleURA
        progTree
        "gListEq(Nil(), gFlatten(lt))"
        "'T'"

-- again, we consider two variants
-- 1) empty answer
sampleURA22b = sampleURA
        progTree
        "gListEq(Nil(), gFlatten(Leaf(x)))"
        "'T'"
-- we abstract
-- l1 = gFlatten(lt)
-- l2 = gFlatten(rt)
-- empty answer
sampleURA22c = sampleURA
        progTree
        "gListEq(Nil(), gAppend(l1, Cons(s, l2)))"
        "'T'"

sampleURA30 = sampleURA
        progList
        "gEqNat(x, S(Z()))"
        "T()"

sampleURA31 = sampleURA
        progList
        "gContains(x, 'a')"
        "T()"

-- this doesn't terminate -
-- but this is non-flat expression!!
sampleURA32 = sampleURA
        progList
        "gEqNat( fSize(x), S(Z()) )"
        "T()"

-- The simplest non-termination of URA for flat function
-- is there a simple way to analyze it??
sampleURA33 = sampleURA
        progList
        "gT(x, T())"
        "F()"


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
    run sampleURA1
    run sampleURA2
    run sampleURA3
    run sampleURA3'
    run sampleURA4
    run sampleURA5
    run sampleURA6
    run sampleURA7
    run sampleURA8
    run sampleURA9
    -- infinite number of answers
    -- run sampleURA10
    -- run sampleURA11
    --run sampleURA12
    run sampleNan1
    run sampleNan2
    run sampleNan3
    run sampleNan4
    run sampleNan5
    run sampleNan6
    run sampleNan7
    return ()
