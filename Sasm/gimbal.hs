import Sasm
import Data.Binary ( Word16, encode )

romVarMap :: StaticVarMap
romVarMap = [
        StaticVar (Var "spi_prescalar"  $ romStaticAddr + 0) 0x04,
        StaticVar (Var "spi_cmd"        $ romStaticAddr + 1) 0x01,
        StaticVar (Var "one"            $ romStaticAddr + 2) 1,
        StaticVar (Var "three"          $ romStaticAddr + 3) 3,
        StaticVar (Var "mpuReadSize"   $ romStaticAddr + 4) 12
    ]

varMap :: VarMap
varMap = [
        Var "spiReadAddress"    $ ramStartAddr + 0,

        Var "accelXH"           $ ramStartAddr + 1,
        Var "accelXL"           $ ramStartAddr + 2,
        Var "accelYH"           $ ramStartAddr + 3,
        Var "accelYL"           $ ramStartAddr + 4,
        Var "accelZH"           $ ramStartAddr + 5,
        Var "accelZL"           $ ramStartAddr + 6,

        Var "gyroXH"            $ ramStartAddr + 7,
        Var "gyroXL"            $ ramStartAddr + 8,
        Var "gyroYH"            $ ramStartAddr + 9,
        Var "gyroYL"            $ ramStartAddr + 10,
        Var "gyroZH"            $ ramStartAddr + 11,
        Var "gyroZL"            $ ramStartAddr + 12,

        Var "accelX"            $ ramStartAddr + 13,
        Var "accelY"            $ ramStartAddr + 14,
        Var "accelZ"            $ ramStartAddr + 15,
        Var "gyroX"             $ ramStartAddr + 16,
        Var "gyroY"             $ ramStartAddr + 17,
        Var "gyroZ"             $ ramStartAddr + 18
    ]

-- Loop:
---- Read gyro+acc
---- Integrate angle position
---- Calc error from 0
---- Calc control signal PWM %
---- Send control signal to the motor through pwm

setupMPU6000 :: Program
setupMPU6000 = [
   -- Set spiPrescalar
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
   Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar"
  ]

readMPU6000 :: Int -> Int -> Program
readMPU6000 r s = [

   -- Set SpiShift to readAddress
   Instruction ""      LOAD      (fromIntegral r :: Word16),
   Instruction ""      STORE    $ varAddress registerMap            "spiShift",

   -- Set SpiCommand to read
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_cmd",
   Instruction ""      STORE   $ varAddress registerMap            "spiCommand",

   -- Loop wait for DONE
   Instruction "done"  LOAD    $ varAddress (fromRom romVarMap)    "three",
   Instruction ""      SUB     $ varAddress registerMap            "spiState",
   Instruction ""      BGZ     $ toLabel (readMPU6000 r s)         "done",

   -- Read Shift
   Instruction ""      LOAD    $ varAddress registerMap            "spiShift",
   Instruction ""      STORE     (fromIntegral s :: Word16)
   ]

readMPU6000AccelXH :: Program
readMPU6000AccelXH = readMPU6000 0x3B (fromIntegral (varAddress varMap "accelXH")) 

readMPU6000AccelXL :: Program
readMPU6000AccelXL = readMPU6000 0x3C (fromIntegral (varAddress varMap "accelXL")) 

readMPU6000AccelYH :: Program
readMPU6000AccelYH = readMPU6000 0x3D (fromIntegral (varAddress varMap "accelYH")) 

readMPU6000AccelYL :: Program
readMPU6000AccelYL = readMPU6000 0x3E (fromIntegral (varAddress varMap "accelYL")) 

readMPU6000AccelZH :: Program
readMPU6000AccelZH = readMPU6000 0x3F (fromIntegral (varAddress varMap "accelZH")) 

readMPU6000AccelZL :: Program
readMPU6000AccelZL = readMPU6000 0x40 (fromIntegral (varAddress varMap "accelZL")) 

gimbal :: Program
gimbal = [
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarMap) "gimbal.hex"
