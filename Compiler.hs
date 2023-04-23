module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter
import Data.Map

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N a) = [LOADI a]
acomp (V a) = [LOAD a]
acomp (Plus a b) = acomp a ++ acomp b ++ [ADD]

--TODO Task 3.2
--Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc a) b c | a == b = [JMP c]
bcomp (Not a) b c | bval a empty /= b = [JMP c]
-- recursive comps which may lead to recursive bvals in which case need to add in a [jmp 1]
bcomp (And (Less x y) z) b c = bcomp (Less x y) b c
bcomp (And x (Less y z)) b c | not b = JMP (c +length (bcomp (Less y z) b c)) : bcomp (Less y z) b c
                             | b = JMP c : bcomp (Less y z) b c
--[JMP 3,LOAD "x",LOADI 5,JMPLESS 3] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) True 3

bcomp (And x y) b c | bval x empty /= b && bval x empty == False = JMP 1 : bcomp y b c
                    | bval x empty /= b && bval x empty == True =  bcomp y b c
                    | bval x empty == b && b == True && bval y empty == b = bcomp x b c ++ bcomp y b c
                    | bval x empty == b && b == False && bval y empty == b = bcomp x b c ++ bcomp y b c
                    | bval x empty == b && b == False && bval y empty == True = bcomp x b c ++ bcomp y b c
-- x and y are arithmetic expressions 
bcomp (Less x y) b c | b =  acomp x ++ acomp y ++ [JMPLESS c]

                     | not b =  acomp x ++ acomp y ++ [JMPGE c]
bcomp a b c = []


--    [JMP 6,LOAD "x",LOADI 5,JMPGE 3] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) False 3




--TODO Task 3.3
-- data Com = Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
ccomp :: Com -> [Instr]
ccomp (Assign x a) = acomp a ++ [STORE x]
ccomp (Seq x y) = ccomp x ++ ccomp y

ccomp (If b x y) =  bcomp b False 5 ++ ccomp x ++ [JMP 2] ++ ccomp y


ccomp (While b x) = bcomp b False 5 ++ ccomp x ++ [JMP (-(length (bcomp b False 5) + length (ccomp x) +1))]









ccomp SKIP = []