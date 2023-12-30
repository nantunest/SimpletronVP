import Sasm
import Data.Binary ( Word16, encode )
import GHC.Core.Utils (scaleAltsBy)
import GHC (GhcException(ProgramError))

type FunctionCall = Address -> Program

romVarDecl :: [StaticVar]
romVarDecl = resolveRomAddr [
        StaticVar "spi_prescalar"  0x04,
        StaticVar "spi_cmd"        0x01,
        StaticVar "one"            1,
        StaticVar "three"          3,
        StaticVar "mpuReadSize"    12,
        StaticVar "eight"          8,
        StaticVar "accelXHaddr"    0x3B,
        StaticVar "accelXLaddr"    0x3C,
        StaticVar "gyroXHaddr"     0x43,
        StaticVar "gyroXLaddr"     0x44,
        StaticVar "MPUdiv"         131,
        StaticVar "fpScale"        6
    ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "spiReadAddress",

        Var "accelXH",
        Var "accelXL",
        Var "gyroXH",
        Var "gyroXL",

        Var "accelX",
        Var "gyroX",

        Var "mulTemp",
        Var "gyroXQ",
        Var "gyroXR",
        Var "accelXQ",
        Var "accelXR",

        Var "gyroXQscaled",
        Var "gyroXRscaled",

        Var "accelXFiP6",
        Var "gyroXFiP6",

        Var "intAccelX",
        Var "intGyroX",

        Var "filtAccelX",
        Var "filtGyroX",

        Var "sensorAngle"

    ]

-- Loop:
---- Read gyro+acc
---- Integrate angle position
---- Calc error from 0
---- Calc control signal PWM %
---- Send control signal to the motor through pwm

inRom :: [Var]
inRom = getVar romVarDecl
inRegisterMap :: VarMap
inRegisterMap = registerMap
inRam :: VarMap
inRam = ramVarDecl

joinFunctionCalls :: [FunctionCall] -> Int -> Program
joinFunctionCalls (p:ps) offset = p (fromIntegral offset) ++ joinFunctionCalls ps (offset + length (p (fromIntegral offset)))
joinFunctionCalls [] _ = []

setupMPU6000 :: Program
setupMPU6000 = [
   -- Set spiPrescalar
   Instruction ""      LOAD    $ valFromAddressOf   "spi_prescalar" inRom,
   Instruction ""      STORE   $ valToAddressOf     "spiPrescalar"  inRegisterMap    
  ]

readMPU6000 :: String -> String -> Address -> Program
readMPU6000 readAddr storeAddr offset = [

   -- Set MPU6000 address to read
   Instruction ""      LOAD    $ valFromAddressOf   readAddr        inRom,
   Instruction ""      STORE   $ valToAddressOf     "spiShift"      inRegisterMap,

   -- Start SPI read
   Instruction ""      LOAD    $ valFromAddressOf   "spi_cmd"       inRom,
   Instruction ""      STORE   $ valToAddressOf     "spiCommand"    inRegisterMap,

   -- Loop wait for DONE
   Instruction "done"  LOAD    $ valFromAddressOf   "three"         inRom,
   Instruction ""      SUB     $ valFromAddressOf   "spiState"      inRegisterMap,

   Instruction ""      BGZ     $ fromIntegral offset +
                                 toLabel
                                (readMPU6000 readAddr storeAddr offset)  "done",

   -- Read and store Shift
   Instruction ""      LOAD    $ valFromAddressOf   "spiShift"      inRegisterMap,
   Instruction ""      SSHR    $ valFromAddressOf   "eight"         inRom,
   Instruction ""      STORE   $ valToAddressOf     storeAddr       ramVarDecl 
   ]

readMPU6000calls :: [FunctionCall]
readMPU6000calls = [
    readMPU6000 "accelXHaddr" "accelXH",
    readMPU6000 "accelXLaddr" "accelXL",
    readMPU6000 "gyroXHaddr"  "gyroXH",
    readMPU6000 "gyroXLaddr"  "gyroXL"
    ]

readMPU6000registers :: Program
readMPU6000registers = joinFunctionCalls readMPU6000calls (length setupMPU6000)

highLowToValue :: String -> String -> String -> Program
highLowToValue h l v = [
    Instruction ""  LOAD  $ valFromAddressOf    h       inRam,
    Instruction ""  SSHL  $ valFromAddressOf    "eight" inRom,
    Instruction ""  SOR   $ valFromAddressOf    l       inRam,
    Instruction ""  STORE $ valToAddressOf      v       inRam
    ]

highLowToValueCalls :: [Program]
highLowToValueCalls = [
    highLowToValue "accelXH" "accelXL" "accelX",
    highLowToValue "gyroXH" "gyroXL" "gyroX"
    ]

concatHighLow :: [Instruction]
concatHighLow = concat highLowToValueCalls

divideByConvFactor :: String -> String -> String -> Program
divideByConvFactor valFromMPU divResult conv = [
       -- Divide valFromMPU by conv, store div result in divResult
       -- divResult = valFromMPU / conv
        Instruction "" LOAD     $ valFromAddressOf  valFromMPU inRam,
        Instruction "" DIV      $ valFromAddressOf  conv inRom,
        Instruction "" STORE    $ valToAddressOf    divResult inRam
    ]

calcReminder :: String -> String -> String -> String -> Program
calcReminder divResult valFromMPU conv divReminder = [
        -- divReminder = valFromMPU - divResult*conv
        Instruction "" LOAD     $ valFromAddressOf divResult inRam,
        Instruction "" MUL      $ valFromAddressOf conv inRom,
        Instruction "" STORE    $ valToAddressOf "mulTemp" inRam,
        Instruction "" LOAD     $ valFromAddressOf valFromMPU inRam,
        Instruction "" SUB      $ valFromAddressOf "mulTemp" inRam,
        Instruction "" STORE    $ valToAddressOf divReminder inRam
        ]

scaleIntegralPart :: String -> String-> String -> Program
scaleIntegralPart divResult scale resultScaled = [
        -- resultScaled = divResult << scale
        Instruction "" LOAD     $ valFromAddressOf divResult    inRam,
        Instruction "" SSHL     $ valFromAddressOf scale        inRom,
        Instruction "" STORE    $ valToAddressOf resultScaled   inRam
        ]

scaleFracPart :: String -> String -> String -> String -> Program
scaleFracPart divReminder fpScale conv fracScaled  = [
        -- fracScaled = divReminder << fpScale
        Instruction "" LOAD     $ valFromAddressOf divReminder  inRam,
        Instruction "" SSHL     $ valFromAddressOf fpScale      inRom,
        Instruction "" DIV      $ valFromAddressOf conv         inRom,
        Instruction "" STORE    $ valFromAddressOf fracScaled   inRam
        ]

assembleFiP6 :: String -> String -> String -> Program
assembleFiP6 i f fip6 = [
    -- fip6 = i | f
    Instruction "" LOAD     $ valFromAddressOf f inRam,
    Instruction "" SOR      $ valFromAddressOf i inRam,
    Instruction "" STORE    $ valFromAddressOf fip6 inRam
    ]

scaleToFiP6 :: Program
scaleToFiP6 = divideByConvFactor    "gyroX"  "gyroXQ"  "MPUdiv"
            ++ calcReminder         "gyroXQ" "gyroX"   "MPUdiv"     "gyroXR"
            ++ scaleIntegralPart    "gyroXQ" "fpScale" "gyroXQscaled"
            ++ scaleFracPart        "gyroXR" "fpScale" "MPUdiv"     "gyroXRscaled"
            ++ assembleFiP6         "gyroXQscaled"  "gyroXRscaled" "gyroXFip6"

-- Alternative to scaleToFiP6
convToFiP6 :: String-> String-> String -> String -> String -> String -> Program
convToFiP6 fromMPU conv q r scale fip6 = [

        -- Divide fromMPU by the MPU6000 conv. factor for +-250 - integral part
        Instruction "" LOAD     $ valFromAddressOf  fromMPU         inRom,
        Instruction "" DIV      $ valFromAddressOf  conv            inRom,
        Instruction "" STORE    $ valToAddressOf    q               inRam,

        -- Calc the reminder of division
        Instruction "" MUL      $ valFromAddressOf  conv            inRom,
        Instruction "" STORE    $ valToAddressOf    "mulTemp"       inRam,
        Instruction "" LOAD     $ valFromAddressOf  fromMPU         inRom,
        Instruction "" SUB      $ valFromAddressOf  "mulTemp"       inRam,
        Instruction "" STORE    $ valToAddressOf    r               inRam,

        -- Scale integral part to Fixed Point scaled
        Instruction "" LOAD     $ valFromAddressOf q                inRam,
        Instruction "" SSHL     $ valFromAddressOf scale            inRom,
        Instruction "" STORE    $ valToAddressOf   fip6             inRam,

        -- Calc reminder scaled to FiP6 and store with gyroAngleFiP6
        Instruction "" LOAD     $ valFromAddressOf r                inRam,
        Instruction "" SSHL     $ valFromAddressOf scale            inRom,
        Instruction "" DIV      $ valFromAddressOf conv             inRom,
        Instruction "" SOR      $ valFromAddressOf fip6             inRam,
        Instruction "" STORE    $ valToAddressOf   fip6             inRam
        ]


integrateVal :: String -> String -> Program
integrateVal var ivar = [
    Instruction ""  LOAD    $ varAddress ramVarDecl    ivar,
    Instruction ""  ADD     $ varAddress ramVarDecl    var,
    Instruction ""  STORE   $ varAddress ramVarDecl    ivar
    ]


integrateValCalls :: [Program]
integrateValCalls = [
    integrateVal "accelX" "intAccelX",
    integrateVal "gyroX" "intGyroX"
    ]

integrateSensors :: [Instruction]
integrateSensors = concat integrateValCalls

gimbal :: [Instruction]
gimbal = setupMPU6000
         ++ readMPU6000registers
         ++ concatHighLow 
         ++ scaleToFiP6
         ++ integrateSensors
         ++ [
            Instruction "" LOAD $ varAddress ramVarDecl "accelX",
            Instruction "" JMP $ fromIntegral (length gimbal - 1)]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarDecl) "gimbal.hex"
