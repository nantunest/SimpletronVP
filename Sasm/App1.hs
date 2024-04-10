module App1 where
import Sasm

romVarDecl :: [StaticVar]
romVarDecl = resolveRomAddr [
        StaticVar "val1"  10
    ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "val2",
        Var "addResult"
    ]

inRom = getVar romVarDecl
inRam = ramVarDecl

app1 :: Program
app1 = [
    Instruction "testApp1"  LOAD    $ valFromAddressOf "val1"    (getVar romVarDecl),
    Instruction ""          ADD     $ valFromAddressOf "val2"    ramVarDecl,
    Instruction ""          STORE   $ valToAddressOf "addResult" ramVarDecl
    ]
