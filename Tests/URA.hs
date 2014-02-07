module Tests.URA (uraTests) where

import Data
import DataUtil
import DataIO
import Driving

import URA
import Examples

import Test.HUnit

-- really smoke testing, since we test external representation (via show)
-- TODO : stronger testing. It requires: reading classes with inequalities,
-- + comparing classes up to variable renaming
testURA name prog input output answer = TestCase $ assertEqual
	name
	answer
	(show (ura (perfectDriveMachine prog) input output))

testURAPartial n name prog input output answer = TestCase $ assertEqual
	name
	answer
	(show (take n (ura (perfectDriveMachine prog) input output)))
------------------------------------------------------
in1 :: Expr
in1 = read "fMatch(x, Cons('A', Cons('B', Nil())))"

out1 :: Expr
out1 = read "'T'"

prog1 = progString

answer1 = "[\
\[(\"x\",Nil())],[(\"x\",Cons('A', Nil()))],\
\[(\"x\",Cons('A', Cons('B', Nil())))],\
\[(\"x\",Cons('B', Nil()))]\
\]"

testURA1 = testURA "testURA1" prog1 in1 out1 answer1
------------------------------------------------------
in2 :: Expr
in2 = read "fMatch(x, Cons('A', Cons('B', Nil())))"

out2 :: Expr
out2 = read "'F'"

prog2 = progString

answer2 = "[\
\[(\"x\",Cons('A', Cons('B', Cons(x.11, x.12))))],\
\[(\"x\",Cons('B', Cons(x.5, x.6)))],\
\[(\"x\",Cons(x.1<!='A', !='B'>, x.2))],\
\[(\"x\",Cons('A', Cons(x.5<!='B'>, x.6)))]\
\]"

testURA2 = testURA "testURA2" prog2 in2 out2 answer2
------------------------------------------------------
in3 :: Expr
in3 = read "gListEq(Cons('a', Cons('b', Cons('c', Cons('d', Cons('e', Cons('f', Cons('g', Nil()))))))), gFlatten(t))"

out3 :: Expr
out3 = read "'T'"

prog3 = progTree

answer3 = "[\
\[(\"t\",Node(Leaf('a'), 'b', Node(Leaf('c'), 'd', Node(Leaf('e'), 'f', Leaf('g')))))],\
\[(\"t\",Node(Leaf('a'), 'b', Node(Node(Leaf('c'), 'd', Leaf('e')), 'f', Leaf('g'))))],\
\[(\"t\",Node(Node(Leaf('a'), 'b', Leaf('c')), 'd', Node(Leaf('e'), 'f', Leaf('g'))))],\
\[(\"t\",Node(Node(Leaf('a'), 'b', Node(Leaf('c'), 'd', Leaf('e'))), 'f', Leaf('g')))],\
\[(\"t\",Node(Node(Node(Leaf('a'), 'b', Leaf('c')), 'd', Leaf('e')), 'f', Leaf('g')))]\
\]"

testURA3 = (testURAPartial 5) "testURA3" prog3 in3 out3 answer3
------------------------------------------------------
in4 :: Expr
in4 = read "gEq(S(Z()), gSize(t))"

out4 :: Expr
out4 = read "'T'"

prog4 = progTree

answer4 = "[[(\"t\",Leaf(t.1))]]"

testURA4 = testURA "testURA4" prog4 in4 out4 answer4
------------------------------------------------------

uraTests = TestList [ testURA1, testURA2, testURA3, testURA4 ]

main = runTestTT uraTests
