module Examples.InterpreterSmallStepCBN where

import Examples.Examples
import Interpreters.SmallStepCBN

example01 = int progList (read "gContains(Nil(), 'A')")
example02 = int progList (read "gContains(Cons('A', Nil()), 'A')")
example03 = int progList (read "gContains(Cons('B', Cons('A', Nil())), 'A')")

example04 = intCount progList (read "gContains(Nil(), 'A')")
example05 = intCount progList (read "gContains(Cons('A', Nil()), 'A')")
example06 = intCount progList (read "gContains(Cons('B', Cons('A', Nil())), 'A')")

