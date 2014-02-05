module Examples.Lambda where

import Data
import Examples.Examples
import Interpreters.MixedCBN2
import DataIO
import Driving
import TreeInterpreter
import URA

buildConfTree prog e = buildProcessTree (perfectDriveMachine prog) e
showTree t = putStrLn (printTree t)

example01 = int progLambda (read "fExample01()")
example02 = int progLambda (read "fExample02()")
example03 = int progLambda (read "fExample03()")
example04 = int progLambda (read "fExample04()")
example05 = int progLambda (read "fExample05()")
example06 = int progLambda (read "fExample06()")

tree01 = buildConfTree progLambda (read "gTypeCheck(x)")
tree02 = buildConfTree progLambda (read "gTypeCheck(App(Var('x', tp1), Var('y', tp2)))")

-- examples of calculation using just tree interpreter
demoIntTree01 = intTree tree01 [("x", read "App(Var('x', Arrow(Base('A'), Base('A'))), Var('y', Base('A')))")]
demoIntTree02 = intTree tree01 [("x", read "App(Var('x', Arrow(Base('B'), Base('A'))), Var('y', Base('A')))")]

sampleURA01 = sampleURA
        "sampleURA01 - expressions are well-typed?"
        progLambda
        "fIsTyped(x)"
        "'T'"

sampleURA02 = sampleURA
        "sampleURA02 - expressions are NOT well-typed?"
        progLambda
        "fIsTyped(x)"
        "'F'"

sampleURA03 = sampleURA
        "sampleURA03 - (x : xt) (y : yt)"
        progLambda
        "fIsTyped(App(Var('x', xt), Var('y', yt)))"
        "'T'"

sampleURA04 = sampleURA
        "sampleURA04 - (x : A -> A) (y : yt)"
        progLambda
        "fIsTyped(App( Var('x', Arrow(Base('A'), Base('A'))) , Var('y', yt)))"
        "'T'"

sampleURA05 = sampleURA
        "sampleURA05 - (x : xt) (y : yt)"
        progLambda
        "fIsTyped(App( Var('x', Arrow(xt, xt)) , Var('y', Base('A'))))"
        "'T'"


-- helper function to run URA demonstration
-- it is a bit tricky in order to indicated not terminated runs
sampleURA :: String -> Program -> String -> String -> IO ()
sampleURA comment prog inputConfText resultText = do
    putStrLn ("\n===================\n"++ comment ++ "\nURA task:")
    putStrLn ("\t" ++ (show input) ++ " -> " ++ (show output))
    putStrLn "answer:"
    putStrLn $ withDelim "\n" $ map (("\t" ++)) $ map prettySub result
    where
        input = (read inputConfText) :: Expr
        output = (read resultText) :: Expr
        result = ura (perfectDriveMachine prog) input output

