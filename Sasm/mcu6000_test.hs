import Sasm

romVarMap :: StaticVarMap
romVarMap = [
        StaticVar (Var "spi_shift"      $ romStaticAddr + 0) 0x05,
        StaticVar (Var "spi_prescalar"  $ romStaticAddr + 1) 0x04,
        StaticVar (Var "spi_cmd"        $ romStaticAddr + 2) 0x01,
        StaticVar (Var "one"            $ romStaticAddr + 3) 1,
        StaticVar (Var "busy_spi_init"  $ romStaticAddr + 4) 20
    ]

varMap :: VarMap
varMap = [
        Var "busy_spi" $ ramStartAddr + 0,
        Var "toread" $ ramStartAddr + 1
    ]

mcu6000test :: Program
mcu6000test = [
    Instruction "init"  LOAD    $ varAddress (fromRom romVarMap)    "spi_shift",
    Instruction ""      STORE   $ varAddress registerMap            "spiShift" ,

    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
    Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar" ,

    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_cmd",
    Instruction ""      STORE   $ varAddress registerMap            "spiCommand",


    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "busy_spi_init",
    Instruction ""      STORE   $ varAddress varMap                 "busy_spi",

    Instruction "cnt"   LOAD    $ varAddress varMap                 "busy_spi",
    Instruction ""      SUB     $ varAddress (fromRom romVarMap)    "one",
    Instruction ""      STORE   $ varAddress varMap                 "busy_spi",
    Instruction ""      BGZ     $ toLabel mcu6000test "cnt",


    Instruction ""      LOAD    $ varAddress registerMap            "spiShift",

    Instruction ""      JMP $ fromIntegral (length mcu6000test - 1)
    ]


main :: IO ()
main = writeAssembledToFile (assembleRom mcu6000test romVarMap) "mcu6000_test.hex"