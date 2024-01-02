import Sasm

romVarDecl :: StaticVarMap
romVarDecl = resolveRomAddr [
        StaticVar "fromMPU"       23658, -- : 180.6 * 131
        StaticVar "MPUdiv"        131,
        StaticVar "fpScale"       6,
        StaticVar "MPUmul"        16,
        StaticVar "compGyro"      57,
        StaticVar "compAcc"       7
   ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "mul1",
        Var "gyroAngleDegQ",
        Var "gyroAngleDegR",
        Var "gyroAngleFiP6"
    ]

inRom :: [Var]
inRom = getVar romVarDecl
inRegisterMap :: VarMap
inRegisterMap = registerMap
inRam :: VarMap
inRam = ramVarDecl


fixedPointTest :: Program
fixedPointTest = [

        -- Divide fromMPU by the MPU6000 conv. factor for +-250 - integral part
        Instruction "" LOAD     $ valFromAddressOf  "fromMPU"       inRom,
        Instruction "" DIV      $ valFromAddressOf  "MPUdiv"        inRom,
        Instruction "" STORE    $ valToAddressOf    "gyroAngleDegQ" inRam,

        -- Calc the reminder of division
        Instruction "" MUL      $ valFromAddressOf  "MPUdiv"        inRom,
        Instruction "" STORE    $ valToAddressOf    "mul1"          inRam,
        Instruction "" LOAD     $ valFromAddressOf  "fromMPU"       inRom,
        Instruction "" SUB      $ valFromAddressOf  "mul1"          inRam,
        Instruction "" STORE    $ valToAddressOf    "gyroAngleDegR" inRam,

        -- Scale integral part to Fixed Point scaled
        Instruction "" LOAD     $ valFromAddressOf "gyroAngleDegQ"  inRam,
        Instruction "" SSHL     $ valFromAddressOf "fpScale"        inRom,
        Instruction "" STORE    $ valToAddressOf   "gyroAngleFiP6"  inRam,

        -- Calc reminder scaled to FiP6 and store with gyroAngleFiP6
        Instruction "" LOAD     $ valFromAddressOf "gyroAngleDegR"  inRam,
        Instruction "" SSHL     $ valFromAddressOf "fpScale"        inRom,
        Instruction "" DIV      $ valFromAddressOf "MPUdiv"         inRom,
        Instruction "" SOR      $ valFromAddressOf "gyroAngleFiP6"  inRam,
        Instruction "" STORE    $ valToAddressOf   "gyroAngleFiP6"  inRam,

        Instruction "" JMP      $ fromIntegral (length fixedPointTest - 1)
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom fixedPointTest romVarDecl) "fixed_point_test.hex"