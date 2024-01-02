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
        StaticVar "fpScale"        6,
        StaticVar "gyroFiltFiP6"   57, -- 0.9 in fixed point 6
        StaticVar "accelFiltFiP6"  7,  -- 0.1 in fixed point 6
    ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "spiReadAddress",

--        Var "accelXH",
--        Var "accelXL",

        -- MPU6000 drive
        Var "gyroXH",
        Var "gyroXL",
        ----------------

        -- Var "accelXfromMPU",

        -- Signals:
        Var "sGyro",
        Var "sPos"
        Var "sControl"
        Var "sError"
        Var "sRef"
        Var "sServoDrive"
        -------------------

        -- Fixed Point Conversion:
        Var "mulTemp",
        Var "qtemp",
        Var "rtemp",
        --------------------------

        -- Var "accelXFiP6",
        Var "gyroXFiP6",

        -- Var "intAccelXFiP6",
        Var "intGyroXFiP6",

        -- Var "filtAccelX",
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

readMPU6000Gyro :: Address -> Program
readMPU6000Gyro offset =  readGyroHigh ++ readGyroLow ++ joinHighLow
                            where readGyroHigh = readMPU6000 "gyroXHaddr" "gyroXH" offset
                                  readGyroLow = readMPU6000 "gyroXLaddr"  "gyroXL" $ (length readGyroHigh) + offset
                                  joinHighLow = highLowToValue "gyroXH" "gyroXL" "gyroX"

readMPU6000calls :: [FunctionCall]
readMPU6000calls = [
--    readMPU6000 "accelXHaddr" "accelXH",
--    readMPU6000 "accelXLaddr" "accelXL",
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
    -- highLowToValue "accelXH" "accelXL" "accelX",
    highLowToValue "gyroXH" "gyroXL" "gyroX"
    ]

concatHighLow :: [Instruction]
concatHighLow = concat highLowToValueCalls

-- Alternative to scaleToFiP6
scaleToFiP6:: String-> String -> String -> String -> Program
scaleToFiP6 fromMPU conv scale fip6 = [

        -- calc quotient of division
        -- q = fromMPU / conv
        Instruction "" LOAD     $ valFromAddressOf  fromMPU         inRom,
        Instruction "" DIV      $ valFromAddressOf  conv            inRom,
        Instruction "" STORE    $ valToAddressOf    "qtemp"         inRam,

        -- Calc the reminder of division
        -- r = fromMPU - q*conv
        Instruction "" MUL      $ valFromAddressOf  conv            inRom,
        Instruction "" STORE    $ valToAddressOf    "mulTemp"       inRam,
        Instruction "" LOAD     $ valFromAddressOf  fromMPU         inRom,
        Instruction "" SUB      $ valFromAddressOf  "mulTemp"       inRam,
        Instruction "" STORE    $ valToAddressOf    "rtemp"         inRam,

        -- Scale integral part to Fixed Point scaled
        -- fip6 = q << scale
        Instruction "" LOAD     $ valFromAddressOf "qtemp"          inRam,
        Instruction "" SSHL     $ valFromAddressOf scale            inRom,
        Instruction "" STORE    $ valToAddressOf   fip6             inRam,

        -- Calc reminder scaled to FiP6 and store with fip6 result 
        -- fip6 = fip6 | ((r << fpScale) / conv)
        Instruction "" LOAD     $ valFromAddressOf "rtemp"          inRam,
        Instruction "" SSHL     $ valFromAddressOf scale            inRom,
        Instruction "" DIV      $ valFromAddressOf conv             inRom,
        Instruction "" SOR      $ valFromAddressOf fip6             inRam,
        Instruction "" STORE    $ valToAddressOf   fip6             inRam
        ]

scaleToFip6Calls :: [Program]
scaleToFip6Calls = [
    scaleToFiP6 "sGyro" "MPUdiv" "fpScale" "gyroXFiP6",
--    scaleToFiP6 "accelXfromMPU" "MPUdiv" "fpScale" "accelXFiP6"
    ]

integrateVal :: String -> String -> Program
integrateVal var ivar = [
    Instruction ""  LOAD    $ varAddress ramVarDecl    ivar,
    Instruction ""  ADD     $ varAddress ramVarDecl    var,
    Instruction ""  STORE   $ varAddress ramVarDecl    ivar
    ]

integrateValCalls :: [Program]
integrateValCalls = [
    integrateVal "gyroXFiP6" "intGyroXFiP6",
    integrateVal "accelXFiP6" "intAccelXFiP6"
    ]


readMPU6000sensors :: [Instruction]
readMPU6000sensors = readMPU6000registers ++ concatHighLow

convertToFixedPoint :: [Instruction]
convertToFixedPoint = concat scaleToFip6Calls

integrateSensors :: [Instruction]
integrateSensors = concat integrateValCalls

filterSensors :: [Instruction]
filterSensors = []

updateActuator :: [Instruction]
updateActuator = []

u

gimbal :: [Instruction]
gimbal =  setupMPU6000
       ++ readMPU6000sensors
       ++ convertToFixedPoint 
       ++ integrateSensors
       ++ [
          Instruction "" LOAD $ varAddress ramVarDecl "accelX",
          Instruction "" JMP $ fromIntegral (length gimbal - 1)]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarDecl) "gimbal.hex"
