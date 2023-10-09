import Sasm

ramStartAddr :: Address
ramStartAddr = 0x0500

romStartAddr :: Address
romStartAddr = 0x0000

pwmStatusReg :: Address
pwmStatusReg = 0xF20

pwmWidthReg :: Address
pwmWidthReg = 0xF21

timerStatusReg :: Address
timerStatusReg = 0x0F10

timerPrescalarReg :: Address
timerPrescalarReg = 0x0F11

timerModulusReg :: Address
timerModulusReg = 0x0F12

timerCountVal :: Address
timerCountVal = 0x0F13

romVal:: RomVarMap
romVal= [
        RomVar (Var "t_modulus" $ romStartAddr - 1) 0x04
    ]

romMap :: [Var]
romMap = [
    Var "t_modulus"     $ romStartAddr + 0,
    Var "t_mode"        $ romStartAddr + 1,
    Var "pwm_width"     $ romStartAddr + 2,
    Var "pwm_mode"      $ romStartAddr + 3
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

    Line "pwmSet"       LOAD $ varAddress varMap "t_modulus",
    Line ""             STORE timerModulusReg,

    Line ""             LOAD $ varAddress varMap "pwm_width",
    Line ""             STORE pwmWidthReg,

    Line ""             LOAD $ varAddress varMap "pwm_mode",
    Line ""             STORE pwmStatusReg,

    Line ""             LOAD $ varAddress varMap "t_mode",
    Line ""             STORE timerStatusReg,

    Line ""             JMP $ fromIntegral (length pwmSetProg - 1)
    ]


program = pwmSetProg

main :: IO ()
main = writeProgToFile program "prog.hex"
