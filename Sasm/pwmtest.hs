import Sasm

romValMap:: StaticVarMap
romValMap= [
        StaticVar (Var "t_modulus" $ romStaticAddr + 0) 10,
        StaticVar (Var "t_mode"    $ romStaticAddr + 1) 2,
        StaticVar (Var "pwm_width" $ romStaticAddr + 2) 7,
        StaticVar (Var "pwm_mode"  $ romStaticAddr + 3) 1
    ]

varMap :: VarMap 
varMap = [
    Var "t_modulus"     $ ramStartAddr + 0,
    Var "t_mode"        $ ramStartAddr + 1,
    Var "pwm_width"     $ ramStartAddr + 2,
    Var "pwm_mode"      $ ramStartAddr + 3
    ]

pwmSetProg :: Program
pwmSetProg = [
    Instruction "pwmSet"       LOAD $ varAddress (fromRom romValMap) "t_modulus",
    Instruction ""             STORE $ varAddress registerMap "timerModulus",

    Instruction ""             LOAD $ varAddress (fromRom romValMap) "pwm_width",
    Instruction ""             STORE $ varAddress registerMap "pwmWidth",

    Instruction ""             LOAD $ varAddress (fromRom romValMap) "pwm_mode",
    Instruction ""             STORE $ varAddress registerMap "pwmState",

    Instruction ""             LOAD $ varAddress (fromRom romValMap) "t_mode",
    Instruction ""             STORE $ varAddress registerMap "timerState",

    Instruction ""             JMP $ fromIntegral (length pwmSetProg - 1)
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom pwmSetProg romValMap) "rom.hex"
