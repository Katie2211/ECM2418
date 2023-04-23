module Main where

import System.Environment
import Compiler
import Interpreter
--TODO Task 3.4
main :: IO ()
main = do c <- getLine
          --print(read(ccomp (read(c:: Interpreter.Com))) :: String)
          print(ccomp (read c :: Com))

          