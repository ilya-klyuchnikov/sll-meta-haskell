module Examples.Examples where

import Data
import DataIO

-- it is safe reading - we read only once
import System.IO.Unsafe

loadProgram :: FilePath -> Program
loadProgram path = read (unsafePerformIO (readFile path))

progString :: Program
progString = loadProgram "Examples/string.sll"

progTree :: Program
progTree = loadProgram "Examples/tree.sll"

progList :: Program
progList = loadProgram "Examples/list.sll"
