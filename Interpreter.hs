{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp = N Val | V String | Plus AExp AExp

    deriving (Eq, Read, Show, Ord)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N a) state = a
aval (V b) state = state ! b
aval (Plus a b) state = aval a state + aval b state -- this should work recursively for howevermany AExp you put into each side of the plus

--TODO Task 2.1
data BExp = Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc b) state = b
bval (Not b) state| (bval b state) = False
                    | not (bval b state) = True
bval (And a b) state| bval a state == True && bval b state == True = True
bval (Less a b) state | aval a state < aval b state = True
bval a b = False -- this acts as an else for some of the statements and a catch all for others

--TODO Task 2.1
data Com = Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP  -- confused over some of the expressions types 
    deriving (Eq, Read, Show)

--TODO Task 2.4
--type State = Map Vname Val

eval :: Com -> State -> State
eval (Assign a b) state = insert a (aval b state) state

eval (Seq a b) state = eval b(eval a state)

eval (If a b c) state | bval a state = eval b state
                      | not (bval a state) = eval c state

--fromList [("x",5)] @=? eval (While (Less (V "x") (N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x",0)])
eval (While a b) state| bval a state = eval (While a b) (eval b state)
                      | not (bval a state) = state
eval SKIP state = state
