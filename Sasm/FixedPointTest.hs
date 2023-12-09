import Sasm

romVarMap :: StaticVarMap
romVarMap = resolveRomAddr [
        StaticVar "fromMPU"       23658, -- : 180.6 * 131
        StaticVar "MPUdiv"        131,
        StaticVar "fpScale"       6,
        StaticVar "MPUmul"        16,
        StaticVar "compGyro"      57,
        StaticVar "compAcc"       7
   ]

varMap :: VarMap
varMap = resolveRamAddr [
        Var "mul1",
        Var "gyroAngleDegQ",
        Var "gyroAngleDegR",
        Var "gyroAngleFiP6"
    ]


fixedPointTest :: Program
fixedPointTest = [

        -- Divide fromMPU by the MPU6000 conv. factor for +-250 - integral part
        Instruction "" LOAD     $ varAddress (fromRom romVarMap) "fromMPU",
        Instruction "" DIV      $ varAddress (fromRom romVarMap) "MPUdiv",
        Instruction "" STORE    $ varAddress varMap "gyroAngleDegQ",

        -- Calc the reminder of division
        Instruction "" MUL      $ varAddress (fromRom romVarMap) "MPUdiv",
        Instruction "" STORE    $ varAddress varMap "mul1",
        Instruction "" LOAD     $ varAddress (fromRom romVarMap) "fromMPU",
        Instruction "" SUB      $ varAddress varMap "mul1",
        Instruction "" STORE    $ varAddress varMap "gyroAngleDegR",

        -- Scale integral part to Fixed Point scaled
        Instruction "" LOAD     $ varAddress varMap "gyroAngleDegQ",
        Instruction "" SSHL     $ varAddress (fromRom romVarMap) "fpScale",
        Instruction "" STORE    $ varAddress varMap "gyroAngleFiP6",

        -- Calc reminder scaled to FiP6 and store with gyroAngleFiP6
        Instruction "" LOAD     $ varAddress varMap "gyroAngleDegR",
        Instruction "" SSHL     $ varAddress (fromRom romVarMap) "fpScale",
        Instruction "" DIV      $ varAddress (fromRom romVarMap) "MPUdiv",
        Instruction "" SOR      $ varAddress varMap "gyroAngleFiP6",
        Instruction "" STORE    $ varAddress varMap "gyroAngleFiP6",

        Instruction "" JMP      $ fromIntegral (length fixedPointTest - 1)
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom fixedPointTest romVarMap) "fixed_point_test.hex"