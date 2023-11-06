import Sasm
    ( assembleRom,
      writeAssembledToFile,
      Program,
      StaticVarMap,
      VarMap )

romVarMap :: StaticVarMap
romVarMap = [
   ]

varMap :: VarMap
varMap = [
   ]

-- Loop:
---- Read gyro x position
---- Calc error
---- Calc control signal
---- Send control signal to the motor through pwm

gimbal :: Program
gimbal = [
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarMap) "gimbal.hex"
