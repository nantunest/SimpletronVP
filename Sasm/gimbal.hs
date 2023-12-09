import Sasm
import Data.Binary ( Word16, encode )
import GHC.Core.Utils (scaleAltsBy)
import GHC (GhcException(ProgramError))

type FunctionCall = Address -> Program

romVarMap :: [StaticVar]
romVarMap = resolveRomAddr [
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
        StaticVar "MPUdiv"         131
    ]

varMap :: [Var]
varMap = resolveRamAddr [
        Var "spiReadAddress",

        Var "accelXH",
        Var "accelXL",
        Var "gyroXH",
        Var "gyroXL",

        Var "accelX",
        Var "gyroX",
        
        Var "accelX",
        Var "gyroX",

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

joinFunctionCalls :: [FunctionCall] -> Int -> Program
joinFunctionCalls (p:ps) offset = p (fromIntegral offset) ++ joinFunctionCalls ps (offset + length (p (fromIntegral offset)))
joinFunctionCalls [] _ = []

setupMPU6000 :: Program
setupMPU6000 = [
   -- Set spiPrescalar
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
   Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar"
  ]

readMPU6000 :: String -> String -> Address -> Program
readMPU6000 readAddr storeAddr offset = [

   -- Set MPU6000 address to read
   Instruction ""      LOAD     $ varAddress (fromRom romVarMap)        readAddr,
   Instruction ""      STORE    $ varAddress registerMap                "spiShift",

   -- Start SPI read
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)         "spi_cmd",
   Instruction ""      STORE   $ varAddress registerMap                 "spiCommand",

   -- Loop wait for DONE
   Instruction "done"  LOAD    $ varAddress (fromRom romVarMap)         "three",
   Instruction ""      SUB     $ varAddress registerMap                 "spiState",
   Instruction ""      BGZ     $ fromIntegral offset +
                                 toLabel
                                (readMPU6000 readAddr storeAddr offset) "done",

   -- Read and store Shift
   Instruction ""      LOAD    $ varAddress registerMap                 "spiShift",
   Instruction ""      SSHR    $ varAddress (fromRom romVarMap)         "eight",
   Instruction ""      STORE   $ varAddress varMap                      storeAddr 
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
    Instruction ""  LOAD  $ varAddress varMap                h,
    Instruction ""  SSHL  $ varAddress (fromRom romVarMap)   "eight",
    Instruction ""  SOR   $ varAddress varMap                l,
    Instruction ""  STORE $ varAddress varMap                v
    ]

highLowToValueCalls :: [Program]
highLowToValueCalls = [
    highLowToValue "accelXH" "accelXL" "accelX",
    highLowToValue "gyroXH" "gyroXL" "gyroX"
    ]

concatHighLow :: [Instruction]
concatHighLow = concat highLowToValueCalls

divideByConvFactor :: String -> String -> Program
divideByConvFactor iVar oVar = [
       -- Divide fromMPU by the MPU6000 conv. factor for +-250 - integral part
        Instruction "" LOAD     $ varAddress (fromRom romVarMap) "fromMPU",
        Instruction "" DIV      $ varAddress (fromRom romVarMap) "MPUdiv",
        Instruction "" STORE    $ varAddress varMap "gyroAngleDegQ"
    ]

calcReminder :: Program
calcReminder = []

scaleIntegralPart :: Program
scaleIntegralPart = []

scaleFracPart :: Program
scaleFracPart = []

scaleToFiP6 :: String -> String -> Program
scaleToFiP6 iVar oVar =  divideByConvFactor iVar oVar
                  ++ calcReminder
                  ++ scaleIntegralPart
                  ++ scaleFracPart

integrateVal :: String -> String -> Program
integrateVal var ivar = [
    Instruction ""  LOAD    $ varAddress varMap    ivar,
    Instruction ""  ADD     $ varAddress varMap    var,
    Instruction ""  STORE   $ varAddress varMap    ivar
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
         ++ scaleToFiP6 "accelX" "accelFiP6"
         ++ integrateSensors
         ++ [
            Instruction "" LOAD $ varAddress varMap "accelX",
            Instruction "" JMP $ fromIntegral (length gimbal - 1)]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarMap) "gimbal.hex"
