module Tests.All where

import Tests.NAN
import Tests.URA

import Test.HUnit

main = do
	runTestTT nanTests
	runTestTT uraTests
