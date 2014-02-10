module Tests.Interpreters (intTests) where

-- tests different interpreters
-- there are tests of two different kinds:
-- 1) testing by example (input - output)
-- 2) testing subtleties of semantics (that some interpreters produce errors)
-- TODO - since all (4) interpreters for each semantics should have the same input/output
-- we may group tests

import Test.HUnit
import Control.Exception

import Examples.Examples
import Data
import DataIO

import qualified Interpreters.BigStepCBV as IntBigStepCBV
import qualified Interpreters.BigStepCBN as IntBigStepCBN
import qualified Interpreters.SmallStepCBV as IntSmallStepCBV
import qualified Interpreters.SmallStepCBN as IntSmallStepCBN

import qualified Interpreters.StagedSmallStepCBN as IntStagedSmallStepCBN
import qualified Interpreters.StagedSmallStepCBV as IntStagedSmallStepCBV
import qualified Interpreters.StagedMixedStepCBN as IntStagedMixedStepCBN
import qualified Interpreters.StagedMixedStepCBV as IntStagedMixedStepCBV

type Interpreter = Program -> Expr -> Expr

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

-- checks input/output relation for execution
testInt :: String -> Interpreter -> Program -> String -> String -> Test
testInt name int prog input output = TestCase $ assertEqual
	name
	(read output)
	(int prog (read input))

-- checks that execution by a given interpreter result in an error
assertIntError :: (Program -> Expr -> Expr) -> Program -> String -> Test
assertIntError int prog input = TestCase $ do
	result <- tryAny (evaluate (int prog (read input)))
	case result of
		Left _ -> return ()
		Right _ -> assertFailure $ "Expected exception: "

-- tests that CBV execution always evals arguments
-- since one argument is illegal (calling non-existing function)
-- CBV execution should result in error
testSSCBV1 =
	assertIntError IntSmallStepCBV.int progTests "fFstArg(Z(), fError(Z()))"
testSSCBV2 =
	assertIntError IntSmallStepCBV.int progTests "fSndArg(fError(Z()), Z())"

testBSCBV1 =
	assertIntError IntBigStepCBV.int progTests "fFstArg(Z(), fError(Z()))"
testBSCBV2 =
	assertIntError IntBigStepCBV.int progTests "fSndArg(fError(Z()), Z())"

testSSSCBV1 =
    assertIntError IntStagedSmallStepCBV.int progTests "fFstArg(Z(), fError(Z()))"
testSSSCBV2 =
    assertIntError IntStagedSmallStepCBV.int progTests "fSndArg(fError(Z()), Z())"

testSMSCBV1 =
    assertIntError IntStagedMixedStepCBV.int progTests "fFstArg(Z(), fError(Z()))"
testSMSCBV2 =
    assertIntError IntStagedMixedStepCBV.int progTests "fSndArg(fError(Z()), Z())"

-- CNB doesn't touch arguments
testSSCBN1 =
	testInt "test1" IntSmallStepCBN.int progTests "fFstArg(Z(), fError(Z()))" "Z()"
testSSCBN2 =
	testInt "test2" IntSmallStepCBN.int progTests "fSndArg(fError(Z()), Z())" "Z()"

testBSCBN1 =
    testInt "test1" IntBigStepCBN.int progTests "fFstArg(Z(), fError(Z()))" "Z()"
testBSCBN2 =
    testInt "test2" IntBigStepCBN.int progTests "fSndArg(fError(Z()), Z())" "Z()"

testSSSCBN1 =
    testInt "test1" IntStagedSmallStepCBN.int progTests "fFstArg(Z(), fError(Z()))" "Z()"
testSSSCBN2 =
    testInt "test2" IntStagedSmallStepCBN.int progTests "fSndArg(fError(Z()), Z())" "Z()"

testSMSCBN1 =
    testInt "test1" IntStagedMixedStepCBN.int progTests "fFstArg(Z(), fError(Z()))" "Z()"
testSMSCBN2 =
    testInt "test2" IntStagedMixedStepCBN.int progTests "fSndArg(fError(Z()), Z())" "Z()"


intTests = TestList [
                    testSSCBV1, testSSCBV2,
                    testBSCBV1, testBSCBV2,
                    testSSSCBV1, testSSSCBV2,
                    testSMSCBV1, testSMSCBV2,
                    testSSCBN1, testSSCBN2,
                    testBSCBN1, testBSCBN2,
                    testSSSCBN1, testSSSCBN2,
                    testSMSCBN1, testSMSCBN2
                    ]

main = runTestTT intTests
