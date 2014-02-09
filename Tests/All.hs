module Tests.All where

import Tests.Interpreters
import Tests.NAN
import Tests.URA

import Test.HUnit

allTests = TestList [intTests, nanTests, uraTests]

main = do
    runTestTT allTests
