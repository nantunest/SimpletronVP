module Gimbal where
import Sasm
import Data.Binary ( Word16, encode )
import Data.List (find)
import Data.Maybe (fromJust)
import GHC.Data.StringBuffer (StringBuffer(len))

type FunctionCall = Address -> Program

romVarDecl :: [StaticVar]
romVarDecl = resolveRomAddr [
        StaticVar "spi_prescalar"  0x04,
        StaticVar "spi_cmd"        0x01,
        StaticVar "tmodulus"       100,
        StaticVar "one"            1,
        StaticVar "three"          3,
        StaticVar "mpuReadSize"    12,
        StaticVar "eight"          8,
        StaticVar "accelXHaddr"    0x3B,
        StaticVar "accelXLaddr"    0x3C,
        StaticVar "gyroHaddr"      0x43,
        StaticVar "gyroLaddr"      0x44,
        StaticVar "MPUdiv"         131,
        StaticVar "MPUscale"       100,
        StaticVar "fpScale"        8,
        StaticVar "gyroFiltFiP6"   57, -- 0.9 in fixed point 6
        StaticVar "accelFiltFiP6"  7,  -- 0.1 in fixed point 6
        StaticVar "sRefStatic"     0,
        StaticVar "ctrlKp"         16, -- 0.25 in FiP6
        StaticVar "ctrlKi"         13, -- 0.20 in FiP6
        StaticVar "semic180"       180,
        StaticVar "totalCycles"    200,
        StaticVar "negMask"        0x8000
    ]

ramVarDecl :: VarMap
ramVarDecl = resolveRamAddr [
        Var "spiReadAddress",

--        Var "accelXH",
--        Var "accelXL",

        -- MPU6000 drive
        Var "gyroH",
        Var "gyroL",
        Var "gyroFromMPU",
        Var "negSign",
        ----------------

        -- Var "accelXfromMPU",

        -- Signals:
        Var "sGyroAcc",
        Var "sGyroVel",
        Var "sGyroPos",
        Var "sControl",
        Var "sError",
        Var "sRef",
        Var "sServoDrive",
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

        Var "sensorAngle",

        Var "integError",
        Var "ctrlResult1",
        Var "ctrlResult2",

        Var "currentCycle"

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
   Instruction "setupMPU6000"      LOAD    $ valFromAddressOf   "spi_prescalar" inRom,
   Instruction ""                  STORE   $ valToAddressOf     "spiPrescalar"  inRegisterMap
  ]

setupPWM :: Program
setupPWM = [
    Instruction "setupPWM"  LOAD    $ valFromAddressOf "tmodulus" inRom,
    Instruction ""          STORE   $ valToAddressOf "timerModulus" inRegisterMap
    ]

readMPU6000 :: String -> String -> Address -> Program
readMPU6000 readAddr storeAddr offset = [

   -- Set MPU6000 address to read
   Instruction ("readMPU6000_" ++ show offset)      LOAD    $ valFromAddressOf   readAddr        inRom,
   Instruction ""      STORE   $ valToAddressOf     "spiShift"      inRegisterMap,

   -- Start SPI read
   Instruction ""      LOAD    $ valFromAddressOf   "spi_cmd"       inRom,
   Instruction ""      STORE   $ valToAddressOf     "spiCommand"    inRegisterMap,

   -- Loop wait for DONE
   Instruction ("tdone_"++show offset) LOAD    $ valFromAddressOf   "three"         inRom,
   Instruction ""      SUB     $ valFromAddressOf   "spiState"      inRegisterMap,

   Instruction ""      BGZ     $ fromIntegral offset +
                                 toLabel
                                (readMPU6000 readAddr storeAddr offset)  ("tdone_"++show offset),

   -- Read and store Shift
   Instruction ""      LOAD    $ valFromAddressOf   "spiShift"      inRegisterMap,
   Instruction ""      SSHR    $ valFromAddressOf   "eight"         inRom,
   Instruction ""      STORE   $ valToAddressOf     storeAddr       inRam
   ]

readMPU6000Gyro :: Address -> Program
readMPU6000Gyro offset =  readGyroHigh ++ readGyroLow ++ joinHighLow
                            where readGyroHigh = readMPU6000 "gyroHaddr" "gyroH" offset
                                  readGyroLow = readMPU6000 "gyroLaddr"  "gyroL" $ fromIntegral (length readGyroHigh) + offset
                                  joinHighLow = highLowToValue "gyroH" "gyroL" "gyroFromMPU"

highLowToValue :: String -> String -> String -> Program
highLowToValue h l v = [
    Instruction "h2l"  LOAD  $ valFromAddressOf    h       inRam,
    Instruction ""  SSHL  $ valFromAddressOf    "eight" inRom,
    Instruction ""  SOR   $ valFromAddressOf    l       inRam,
    Instruction ""  STORE $ valToAddressOf      v       inRam
    -- Instruction ""  SAND  $ valFromAddressOf "negMask" inRom,
    -- Instruction ""  BEZ 0, --branch to end of procedure
    -- Instruction ""  LOAD $ valFromAddressOf v inRam,
    -- Instruction ""  SNOT 0,
    -- Instruction ""  SUB $ valFromAddressOf "one" inRom
    ]

-- Alternative to scaleToFiP6
scaleToFiP6:: String-> String -> String -> String -> Address -> Program
scaleToFiP6 fromMPU conv scale fip6 offset = [

        Instruction ""           LOAD     $ valFromAddressOf  fromMPU         inRam,
        Instruction ""           SAND     $ valFromAddressOf  "negMask"       inRom,
        Instruction ""           STORE    $ valToAddressOf    "negSign"       inRam,
        Instruction ""           BEZ      $ fromIntegral offset + toLabel
                                            (scaleToFiP6 fromMPU conv scale fip6 offset)
                                            "scaleToFip", --- if negative remove sign, else goto scale to fip

        Instruction ""           LOAD     $ valFromAddressOf fromMPU          inRam,
        Instruction ""           SUB      $ valFromAddressOf "one"            inRom,
        Instruction ""           SNOT     0,
        Instruction ""           STORE    $ valToAddressOf fromMPU            inRam,

        -- calc quotient of division
        -- q = fromMPU / conv
        Instruction "scaleToFip" LOAD     $ valFromAddressOf  fromMPU         inRam,
        Instruction ""           DIV      $ valFromAddressOf  conv            inRom,
        Instruction ""           STORE    $ valToAddressOf    "qtemp"         inRam,

        -- Calc the reminder of division
        -- r = fromMPU - q*conv
        Instruction ""           MUL      $ valFromAddressOf  conv            inRom,
        Instruction ""           STORE    $ valToAddressOf    "mulTemp"       inRam,
        Instruction ""           LOAD     $ valFromAddressOf  fromMPU         inRam,
        Instruction ""           SUB      $ valFromAddressOf  "mulTemp"       inRam,
        Instruction ""           STORE    $ valToAddressOf    "rtemp"         inRam,

        -- Scale integral part to Fixed Point scaled
        -- fip6 = q << scale
        Instruction ""           LOAD     $ valFromAddressOf "qtemp"          inRam,
        Instruction ""           SSHL     $ valFromAddressOf scale            inRom,
        Instruction ""           STORE    $ valToAddressOf   fip6             inRam,

        -- Calc reminder scaled to FiP6 and store with fip6 result 
        -- fip6 = fip6 | ((r << fpScale) / conv)
        Instruction ""           LOAD     $ valFromAddressOf "rtemp"          inRam,
        Instruction ""           SSHL     $ valFromAddressOf scale            inRom,
        Instruction ""           DIV      $ valFromAddressOf conv             inRom,
        Instruction ""           SOR      $ valFromAddressOf fip6             inRam,
        Instruction ""           STORE    $ valToAddressOf   fip6             inRam,

        -- Add sign bit again if the number was negative
        Instruction ""           LOAD     $ valFromAddressOf "negSign"        inRam,
        Instruction ""           BEZ      $ fromIntegral offset +
                                            toLabel
                                            (scaleToFiP6 fromMPU conv scale fip6 offset)
                                            "scaleToFipEnd",

        Instruction ""           LOAD     $ valFromAddressOf fip6             inRam,
        Instruction ""           SNOT     0,
        Instruction ""           ADD      $ valFromAddressOf "one"            inRom,
        Instruction ""           STORE    $ valToAddressOf fip6               inRam,
        Instruction ""           LOAD     $ valFromAddressOf fromMPU             inRam,
        Instruction ""           SNOT     0,
        Instruction ""           ADD      $ valFromAddressOf "one"            inRom,
        Instruction ""           STORE    $ valToAddressOf fromMPU               inRam,
 
        Instruction "scaleToFipEnd" LOAD     $ valFromAddressOf fip6          inRam
        ]

integrateVal :: String -> String -> Program
integrateVal var ivar = [
    Instruction "integ"  LOAD    $ varAddress ramVarDecl    ivar,
    Instruction ""  ADD     $ varAddress ramVarDecl    var,
    Instruction ""  STORE   $ varAddress ramVarDecl    ivar
    ]


mulFip6 :: String -> String -> String -> Address -> [Instruction]
mulFip6 a b c offset = [

    Instruction ""      LOAD    $ valFromAddressOf a         inRam,
    Instruction ""      SAND    $ valFromAddressOf "negMask" inRom,
    Instruction ""      BEZ     $ fromIntegral offset +
                                            toLabel
                                            (mulFip6 a b c offset)
                                            "mul",

    Instruction ""      LOAD    $ valFromAddressOf a         inRam,
    Instruction ""      SUB     $ valFromAddressOf "one"     inRom,
    Instruction ""      SNOT    0,

    Instruction "mul"   MUL     $ valFromAddressOf b         inRom,
    Instruction ""      SSHR    $ valFromAddressOf "fpScale" inRom,
    Instruction ""      STORE   $ valToAddressOf   c         inRam,

    Instruction ""      LOAD    $ valFromAddressOf a         inRam,
    Instruction ""      SAND    $ valFromAddressOf "negMask" inRom,
    Instruction ""      BEZ     $ fromIntegral offset +
                                            toLabel
                                            (mulFip6 a b c offset)
                                            "fin",

    Instruction ""      LOAD    $ valFromAddressOf c         inRam,
    Instruction ""      SNOT    0,
    Instruction ""      ADD     $ valFromAddressOf "one"     inRom,
    Instruction "fin"   LOAD    $ valFromAddressOf c         inRam

    ]

errorP :: Program
errorP = [
    Instruction "errorP" LOAD     $ valFromAddressOf  "sRefStatic"    inRom,
    Instruction "" SUB      $ valFromAddressOf  "sGyroPos"      inRam,
    Instruction "" STORE    $ valToAddressOf    "sError"        inRam
    ]

-- multFiP6 :: Program
-- multFip6 = []

controlP :: Address -> Program
controlP offset = [
    -- (ctrlResul1 = (kp * e)) + (ctrlResult2 = (ki * integ (e)))

    -- integrate error
    Instruction "controlP" LOAD     $ valFromAddressOf  "sError"      inRam,
    Instruction "" ADD      $ valFromAddressOf  "integError"  inRam,
    Instruction "" STORE    $ valToAddressOf    "integError"  inRam
    ]
    ++

    mulFip6 "integError" "ctrlKi" "ctrlResult2" (fromIntegral offset + 3)

    ++

    mulFip6 "sError" "ctrlKp" "ctrlResult1" (fromIntegral offset + 3 + fromIntegral (length $ mulFip6 "integError" "ctrlKi" "ctrlResult2" 0))
    ++
    [

    -- add parts
    Instruction "" ADD      $ valFromAddressOf  "ctrlResult2" inRam,
    Instruction "" STORE    $ valToAddressOf    "sControl"    inRam

    -- Instruction "" LOAD     $ valFromAddressOf "gyroFromMPU" inRam,
    -- Instruction "" LOAD     $ valFromAddressOf "sGyroAcc" inRam,

    ]

traceSignal :: String -> Program
traceSignal sig = [
    Instruction "" LOAD     $ valFromAddressOf sig inRam,
    Instruction "" PDBG 0
    ]

convertGyroToFiP6 :: Address -> Program
convertGyroToFiP6 = scaleToFiP6 "gyroFromMPU" "MPUdiv" "fpScale" "sGyroAcc"

integrateGyro2 :: Program
integrateGyro2 = integrateVal "sGyroAcc" "sGyroVel" ++ integrateVal "sGyroVel" "sGyroPos" ++ traceSignal "sGyroPos"

updateActuator :: Program
updateActuator = [
    Instruction "updateAct" LOAD $ valFromAddressOf "sControl" inRam,
    Instruction "" DIV  $ valFromAddressOf "semic180" inRom,
    Instruction "" SSHR $ valFromAddressOf "fpScale"  inRom,
    Instruction "" STORE $ valToAddressOf "pwmWidth" inRegisterMap
    ]

testLoop :: Program
testLoop = [
    Instruction "testLoop"  LOAD $ valFromAddressOf "currentCycle" inRam,
    Instruction ""          ADD $ valFromAddressOf "one"           inRom,
    Instruction ""          STORE $ valToAddressOf "currentCycle"  inRam,
    Instruction ""          LOAD $ valFromAddressOf "totalCycles"  inRom,
    Instruction ""          SUB $ valFromAddressOf "currentCycle"  inRam,
    Instruction ""          BGZ 4
    ]

gimbal :: [Instruction]
gimbal =  setupMPU6000
       ++ setupPWM
       ++ readMPU6000Gyro (fromIntegral (length setupMPU6000 + length setupPWM))
       ++ convertGyroToFiP6 (fromIntegral (length setupMPU6000 + length setupPWM + length (readMPU6000Gyro (fromIntegral (length setupMPU6000 + length setupPWM))) ))
       ++ integrateGyro2
       ++ errorP
       ++ controlP (fromIntegral (length setupMPU6000 + length setupPWM + length integrateGyro2 + length errorP + length ( convertGyroToFiP6 (fromIntegral (length setupMPU6000 + length setupPWM + length (readMPU6000Gyro (fromIntegral (length setupMPU6000 + length setupPWM))) )))  ))
       ++ updateActuator
       ++ testLoop
       ++ [Instruction "end" JMP $ fromIntegral (length gimbal - 1)]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarDecl) "Gimbal.hex"

prepareProgStr :: Program -> Program -> [String]
-- prettyPrint = map ((show) . (\(Instruction l i o) -> (i, o)))

prepareProgStr' (i:is) prog = (show label ++ addTabs label ++ show opCode ++ "\t" ++ show (findVar addr) ++ "\n") : prepareProgStr' is prog
                        where label = (\(Instruction l i o) -> l) i
                              opCode = (\(Instruction l i o) -> i) i
                              addr = (\(Instruction l i o) -> o) i
                              findVar a | a < romStaticAddr = findLabel a
                                        | a >= romStaticAddr && a < ramStartAddr = findVarInRom a
                                        | a >= ramStartAddr && a < regMapStartAddr = findVarInRam a
                                        | a >= regMapStartAddr = findReg a
                                        | otherwise = show a
                              findLabel a = labelName (prog!!fromIntegral addr) addr
                              findVarInRom a = romVarName $ romVarDecl!!(fromIntegral a - fromIntegral romStaticAddr)
                              findVarInRam a = varName $ ramVarDecl!!(fromIntegral a - fromIntegral ramStartAddr)
                              findReg a = varName $ fromJust $ find (\(Var ni ai) -> a == ai) registerMap
                              romVarName (StaticVar n v a) = n
                              varName (Var n a) = n
                              labelName (Instruction l i o) a | l == "" = show a
                                                              | otherwise = l
                              addTabs l | length l >= 14 = "\t"
                                        | length l >= 6 = "\t\t"
                                        | otherwise = "\t\t\t"
prepareProgStr [] _ = []

prettyPrint :: [Instruction] -> String
prettyPrint prog = concat $ addLine $ addTab $ prepareProgStr' prog prog
                    where addLine = zipWith (++) (map show [0..])
                          addTab = zipWith (++) (replicate progLen "\t")
                          progLen = length $ prepareProgStr' prog prog
