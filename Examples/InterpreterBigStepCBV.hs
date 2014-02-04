module Examples.InterpreterBigStepCBN where

import Examples.Examples
import Interpreters.BigStepCBV

example01 = int progList (read "gContains(Nil(), 'A')")
example02 = int progList (read "gContains(Cons('A', Nil()), 'A')")
example03 = int progList (read "gContains(Cons('B', Cons('A', Nil())), 'A')")

example04 = int progTest (read "fFirstArg('a', fInf())")
example05 = int progTest (read "fFirstArg('a', fLoop())")
