module Tests.NAN (nanTests) where

import Data
import DataUtil
import DataIO
import Driving

import NAN
import Examples

import Test.HUnit

-- regression tests for NAN

-- really smoke testing, since we test external representation (via show)
-- TODO : comparing classes up to variable renaming
testNAN name prog input center answer = TestCase $ assertEqual
	name
	answer
	(show (nan (perfectDriveMachine prog) input center))

------------------------------------------------------
in1 :: Expr
in1 = read "fEq(x, y)"

center1 :: Expr
center1 = read "fEq('A', 'A')"

prog1 = progString

answer1 = "[(\"x\",y),(\"y\",y)]"

testNAN1 = testNAN "testNAN1" prog1 in1 center1 answer1
------------------------------------------------------
in2 :: Expr
in2 = read "fEq(x, y)"

center2 :: Expr
center2 = read "fEq('A', 'B')"

prog2 = progString

answer2 = "[(\"x\",x<!=y>),(\"y\",y<!=x>)]"

testNAN2 = testNAN "testNAN2" prog2 in2 center2 answer2
------------------------------------------------------
in3 :: Expr
in3 = read "fMatch(x, y)"

center3 :: Expr
center3 = read "fMatch(Cons('A', Nil()), Cons('A', Cons('A', Nil())))"

prog3 = progString

answer3 = "[(\"x\",Cons(x.1, Nil())),(\"y\",Cons(x.1, y.2))]"

testNAN3 = testNAN "testNAN3" prog3 in3 center3 answer3
------------------------------------------------------
in4 :: Expr
in4 = read "fMatch(x, y)"

center4 :: Expr
center4 = read "fMatch(Cons('A', Nil()), Cons('B', Cons('A', Nil())))"

prog4 = progString

answer4 = "[(\"x\",Cons(x.1<!=y.1>, Nil())),(\"y\",Cons(y.1<!=x.1>, Cons(x.1<!=y.1>, y.6)))]"

testNAN4 = testNAN "testNAN4" prog4 in4 center4 answer4
------------------------------------------------------
in5 :: Expr
in5 = read "P(gStrEq(x, Cons('B', Nil())))"

center5 :: Expr
center5 = read "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())))"

prog5 = progString

answer5 = "[(\"x\",Cons(x.1<!='B'>, x.2))]"

testNAN5 = testNAN "testNAN5" prog5 in5 center5 answer5
------------------------------------------------------
in6 :: Expr
in6 = read "P(gStrEq(x, Cons('B', Nil())), gStrEq(x, Cons('C', Nil())))"

center6 :: Expr
center6 = read "P(gStrEq(Cons('A', Nil()), Cons('B', Nil())), gStrEq(Cons('A', Nil()), Cons('C', Nil())))"

prog6 = progString

answer6 = "[(\"x\",Cons(x.1<!='B', !='C'>, x.2))]"

testNAN6 = testNAN "testNAN6" prog6 in6 center6 answer6
------------------------------------------------------
in7 :: Expr
in7 = read "P(x, fEq(x, y))"

center7 :: Expr
center7 = read "P('A', fEq('A', 'B'))"

prog7 = progString

answer7 = "[(\"x\",'A'),(\"y\",y<!='A'>)]"

testNAN7 = testNAN "testNAN7" prog7 in7 center7 answer7

nanTests = TestList [ testNAN1, testNAN2, testNAN3, testNAN4, testNAN5, testNAN6, testNAN7 ]

main = runTestTT nanTests
