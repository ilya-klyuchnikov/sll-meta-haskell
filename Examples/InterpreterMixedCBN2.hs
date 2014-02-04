module Examples.InterpreterMixedCBN2 where

import Examples.Examples
import Interpreters.MixedCBN2
import DataIO

example01 = int progList (read "gContains(Nil(), 'A')")
example02 = int progList (read "gContains(Cons('A', Nil()), 'A')")
example03 = int progList (read "gContains(Cons('B', Cons('A', Nil())), 'A')")

example04 = int progTest (read "fFirstArg('a', fInf())")
example05 = int progTest (read "fFirstArg('a', fLoop())")

example06 = int progTest (read "fExample06()")

tree01 = buildTree progList (read "gContains(Nil(), 'A')")
tree02 = buildTree progList (read "gContains(Cons('A', Nil()), 'A')")
tree03 = buildTree progList (read "gContains(Cons('B', Cons('A', Nil())), 'A')")
tree04 = buildTree progTest (read "fFirstArg('a', fInf())")
tree05 = buildTree progTest (read "fFirstArg('a', fLoop())")
tree06 = buildTree progTest (read "fExample06()")

showTree t = putStrLn (printTree t)
