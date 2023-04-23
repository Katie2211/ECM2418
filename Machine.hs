{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where


import Prelude hiding (lookup)
import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val


--TODO Task 1.4
data Instr = LOADI Val | LOAD Vname | ADD | STORE Vname | JMP Val | JMPLESS Val | JMPGE Val
        --IUndefined
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]


--TODO Task 1.6
type Config = (Int, State, Stack) -- the int here is a counter

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI f) (a, state, ds) = (a+1, state, f:ds)-- this will include the base case of an empty list

iexec (LOAD f) (a, state, ds) = (a+1, state, state ! f :ds)

iexec ADD (a, state, []) = (a+1, state, []) -- here it handles if the list doesnt have any elements
iexec ADD (a, state, [d]) = (a+1, state, [d]) -- here it handles if the list doesnt have enough elements
iexec ADD (a, state, d:e:ds) = (a+1,  state , (d+e):ds)

iexec (STORE f) (a, state, []) = (a+1, state, [])
iexec (STORE f) (a, state, d:ds) = (a+1, insert f d state , ds)

iexec (JMP f) (a, state, []) = (a+f+1, state, [])
iexec (JMP f) (a, state, ds) = (a+f+1, state, ds)

iexec (JMPLESS f) (a, state, []) = (a +1, state, [])
iexec (JMPLESS f) (a, state, d:e:ds)  |e<d = (a+f+1,  state, ds)
                                      |e>=d = (a +1, state, ds)
iexec (JMPLESS f) (a, state, [d]) = (a +1, state, [d])

iexec (JMPGE f) (a, state, []) = (a +1, state, [])
iexec (JMPGE f) (a, state, [d]) = (a +1, state, [d])
iexec (JMPGE f) (a, state, d:e:ds) |e>=d = (a+f+1,  state, ds)
                                   |e< d = (a +1, state, ds)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (a,state,bs) = (a, state, bs)
exec [y] (a, state, bs) = iexec y (a, state, bs)
exec (y:ys) (a, state, bs) = exec ys (iexec y (a, state, bs))

