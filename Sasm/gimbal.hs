import Sasm

romVarMap :: StaticVarMap
romVarMap = [
   ]

varMap :: VarMap
varMap = [
   ]

gimbal :: Program
gimbal = [
    ]

main :: IO ()
main = writeAssembledToFile (assembleRom gimbal romVarMap) "gimbal.hex"
