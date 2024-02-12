module App2 where
import Sasm

romVarDecl :: [StaticVar]
romVarDecl = resolveRomAddr [
        StaticVar "vtrue"  1,
        StaticVar "vfalse" 0
    ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "a",
        Var "b",
        Var "c",
        Var "a2",
        Var "b2",
        Var "c2",
        Var "a2b2",
        Var "result"
    ]

inRom = getVar romVarDecl
inRam = ramVarDecl

calcSquare :: String -> String -> Program
calcSquare n n2 = [
    Instruction "calcSquare"  LOAD    $ valFromAddressOf  n     inRam,
    Instruction ""            MUL     $ valFromAddressOf  n     inRam,
    Instruction ""            STORE   $ valToAddressOf    n2    inRam
    ]

sum2 :: String -> String -> String -> Program
sum2 n1 n2 res = [
    Instruction "sum2"  LOAD  $ valFromAddressOf n1  inRam,
    Instruction ""      ADD   $ valFromAddressOf n2  inRam,
    Instruction ""      STORE $ valToAddressOf   res inRam
    ]

app2 :: Program
app2 = calcSquare "a" "a2"
    ++ calcSquare "b" "b2"
    ++ calcSquare "c" "c2"
    ++ sum2 "a2" "b2" "a2b2"
    ++ [
        Instruction "cmp"    SUB   $ valFromAddressOf   "c2"     inRam,
        Instruction ""       BEZ   $ toLabel app2       "ltrue",   
        Instruction ""       LOAD  $ valFromAddressOf   "vfalse" inRom,
        Instruction ""       STORE $ valToAddressOf     "result" inRam,
        Instruction ""       JMP   $ toLabel app2       "end",
        Instruction "ltrue"  LOAD  $ valFromAddressOf   "vtrue"  inRom,
        Instruction ""       STORE $ valToAddressOf     "result" inRam,
        Instruction "end"    PDBG  0,
        Instruction ""       JMP   $ toLabel app2       "end"
    ]