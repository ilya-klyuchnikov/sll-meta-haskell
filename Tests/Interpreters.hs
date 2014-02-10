module Tests.Interpreters (intTests) where

-- tests different interpreters
-- there are tests of two different kinds:
-- 1) testing by example (input - output)
-- 2) testing subtleties of semantics (that some interpreters produce errors)

import Test.HUnit
import Control.Exception

import Examples.Examples
import Data
import DataIO

import qualified Interpreters.BigStepCBN as IntBigStepCBN
import qualified Interpreters.SmallStepCBN as IntSmallStepCBN
import qualified Interpreters.StagedSmallStepCBN as IntStagedSmallStepCBN
import qualified Interpreters.StagedMixedStepCBN as IntStagedMixedStepCBN

import qualified Interpreters.BigStepCBV as IntBigStepCBV
import qualified Interpreters.SmallStepCBV as IntSmallStepCBV
import qualified Interpreters.StagedSmallStepCBV as IntStagedSmallStepCBV
import qualified Interpreters.StagedMixedStepCBV as IntStagedMixedStepCBV

type Interpreter = Program -> Expr -> Expr

callByValueInterpreters =
    [IntBigStepCBV.int, IntSmallStepCBV.int, IntStagedSmallStepCBV.int, IntStagedMixedStepCBV.int]
callByNameInterpreters  =
    [IntBigStepCBN.int, IntSmallStepCBN.int, IntStagedSmallStepCBN.int, IntStagedMixedStepCBN.int]

-- checks input/output relation for execution
assertIntInputOutput :: String -> Interpreter -> Program -> String -> String -> Test
assertIntInputOutput name int prog input output = TestCase $ assertEqual
	name
	(read output)
	(int prog (read input))

-- checks that execution by a given interpreter result in an error
assertIntError :: Interpreter -> Program -> String -> Test
assertIntError int prog input = TestCase $ do
	result <- tryAny (evaluate (int prog (read input)))
	case result of
		Left _ -> return ()
		Right _ -> assertFailure $ "Expected exception: "
    where
        -- catching all exceptions
        tryAny :: IO a -> IO (Either SomeException a)
        tryAny = try

assertCallByNameIntInputOutput :: String -> Program -> String -> String -> Test
assertCallByNameIntInputOutput name program input output =
    TestList [assertIntInputOutput name int program input output | int <- callByNameInterpreters]

assertCallByValueIntInputOutput :: String -> Program -> String -> String -> Test
assertCallByValueIntInputOutput name program input output =
    TestList [assertIntInputOutput name int program input output | int <- callByValueInterpreters]

assertCallByNameIntError :: String -> Program -> String -> Test
assertCallByNameIntError name program input =
    TestList [assertIntError int program input | int <- callByNameInterpreters]

assertCallByValueIntError :: String -> Program -> String -> Test
assertCallByValueIntError name program input =
    TestList [assertIntError int program input | int <- callByValueInterpreters]

-- tests that CBV execution always evals arguments
-- since one argument is illegal (calling non-existing function)
-- CBV execution should result in error
testCBV1 = assertCallByValueIntError "testCBV1" progTests "fFstArg(Z(), fError(Z()))"
testCBV2 = assertCallByValueIntError "testCBV2" progTests "fSndArg(fError(Z()), Z())"

-- CBN doesn't touch arguments which are not needed for answer
testCBN1 = assertCallByNameIntInputOutput "testCBN1" progTests "fFstArg(Z(), fError(Z()))" "Z()"
testCBN2 = assertCallByNameIntInputOutput "testCBN2" progTests "fSndArg(fError(Z()), Z())" "Z()"

-- CBN DOES touch arguments which are not needed for answer
testCBN3 = assertCallByNameIntError "testCBN3" progTests "fSndArg(Z(), fError(Z()))"
testCBN4 = assertCallByNameIntError "testCBN4" progTests "fFstArg(fError(Z()), Z())"


intTests = TestList [testCBN1, testCBN2, testCBN3, testCBN4, testCBV1, testCBV2]

main = runTestTT intTests
