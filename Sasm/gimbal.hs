import Sasm
import Data.Binary ( Word16, encode )
import GHC.CmmToAsm.X86.Instr (Instr(SHR))

romVarMap :: StaticVarMap
romVarMap = [
        StaticVar (Var "spi_prescalar"  $ romStaticAddr + 0) 0x04,
        StaticVar (Var "spi_cmd"        $ romStaticAddr + 1) 0x01,
        StaticVar (Var "one"            $ romStaticAddr + 2) 1,
        StaticVar (Var "three"          $ romStaticAddr + 3) 3,
        StaticVar (Var "mpuReadSize"    $ romStaticAddr + 4) 12,
        StaticVar (Var "eight"          $ romStaticAddr + 5) 8
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

joinPrograms :: [Int -> Program] -> Int -> Program
joinPrograms (p:ps) offset = p offset ++ joinPrograms ps (offset + length (p offset))
joinPrograms [] _ = []

setupMPU6000 :: Program
setupMPU6000 = [
   -- Set spiPrescalar
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
   Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar"
  ]

readMPU6000 :: Int -> Int -> Int -> Program
readMPU6000 r s offset = [

   -- Set SpiShift to readAddress
   Instruction ""      LOAD      (fromIntegral r :: Word16),
   Instruction ""      STORE    $ varAddress registerMap                "spiShift",

   -- Set SpiCommand to read
   Instruction ""      LOAD    $ varAddress (fromRom romVarMap)         "spi_cmd",
   Instruction ""      STORE   $ varAddress registerMap                 "spiCommand",

   -- Loop wait for DONE
   Instruction "done"  LOAD    $ varAddress (fromRom romVarMap)         "three",
   Instruction ""      SUB     $ varAddress registerMap                 "spiState",
   Instruction ""      BGZ     $ fromIntegral offset +
                                 toLabel (readMPU6000 r s offset)       "done",

   -- Read and store Shift
   Instruction ""      LOAD    $ varAddress registerMap                 "spiShift",
   Instruction ""      SSHR     $ varAddress (fromRom romVarMap)        "eight",
   Instruction ""      STORE     (fromIntegral s :: Word16)
   ]

readMPU6000Accel :: Int -> String -> Int -> Program
readMPU6000Accel r l = readMPU6000 r (fromIntegral (varAddress varMap l))

readMPU6000AccelList :: [Int -> Program]
readMPU6000AccelList = [
    readMPU6000Accel 0x3B "accelXH",
    readMPU6000Accel 0x3C "accelXL",
    readMPU6000Accel 0x3D "accelYH",
    readMPU6000Accel 0x3E "accelYL",
    readMPU6000Accel 0x3F "accelZH",
    readMPU6000Accel 0x40 "accelZL" 
    ]

gimbal :: Program
gimbal = [
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarMap) "gimbal.hex"
