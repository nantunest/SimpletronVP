import Sasm

romValMap:: RomVarMap
romValMap= [
        RomVar (Var "t_modulus" $ romStaticAddr + 0) 10,
        RomVar (Var "t_mode"    $ romStaticAddr + 1) 2,
        RomVar (Var "pwm_width" $ romStaticAddr + 2) 7,
        RomVar (Var "pwm_mode"  $ romStaticAddr + 3) 1
    ]

varMap :: [Var]
varMap = [
    Var "t_modulus"     $ ramStartAddr + 0,
    Var "t_mode"        $ ramStartAddr + 1,
    Var "pwm_width"     $ ramStartAddr + 2,
    Var "pwm_mode"      $ ramStartAddr + 3
    ]

pwmSetProg :: [Line]
pwmSetProg = [
    Line "pwmCofig"     READ $ varAddress varMap "t_modulus",
    Line ""             READ $ varAddress varMap "t_mode",
    Line ""             READ $ varAddress varMap "pwm_width",
    Line ""             READ $ varAddress varMap "pwm_mode",

    Line "pwmSet"       LOAD $ varAddress (fromRom romValMap) "t_modulus",
    Line ""             STORE timerModulusReg,

    Line ""             LOAD $ varAddress varMap "pwm_width",
    Line ""             STORE pwmWidthReg,

    Line ""             LOAD $ varAddress varMap "pwm_mode",
    Line ""             STORE pwmStatusReg,

    Line ""             LOAD $ varAddress varMap "t_mode",
    Line ""             STORE timerStatusReg,

    Line ""             JMP $ fromIntegral (length pwmSetProg - 1)
    ]

main :: IO ()
main = writeAssembledToFile (assembleProgram pwmSetProg) "prog.hex"
