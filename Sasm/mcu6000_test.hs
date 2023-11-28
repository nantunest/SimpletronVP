import Sasm

romVarMap :: StaticVarMap
romVarMap = resolveRomAddr [
        StaticVar "spi_prescalar" 0x04,
        StaticVar "spi_cmd"       0x01,
        StaticVar "one"           1,
        StaticVar "three"         3,
        StaticVar "finalAddress"  14
    ]

varMap :: VarMap
varMap = resolveRamAddr [
        Var "spiReadAddress"
    ]

-- 01 set spiPrescalar
-- 02 Set SpiShift to readAddress
-- 03 Set SpiCommand to read
-- 04.1 Loop wait for DONE
-- 04.2 Read Shift
-- 05 Increase readAddress
-- 06 ReadAddress > finalAddress ? Jump to finish
-- 07 Jump to 02

mcu6000test :: Program
mcu6000test = [

-- 01 set spiPrescalar
    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
    Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar",

-- 02 Set SpiShift to readAddress
    Instruction "spir"  LOAD    $ varAddress varMap                 "spiReadAddress",
    Instruction ""      STORE   $ varAddress registerMap            "spiShift" ,

-- 03 Set SpiCommand to read
    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_cmd",
    Instruction ""      STORE   $ varAddress registerMap            "spiCommand",

-- 04.1 Loop wait for DONE
    Instruction "done"  LOAD    $ varAddress (fromRom romVarMap)    "three",
    Instruction ""      SUB     $ varAddress registerMap            "spiState",
    Instruction ""      BGZ     $ toLabel mcu6000test               "done",

-- 04.2 Read Shift
    Instruction ""      LOAD    $ varAddress registerMap            "spiShift",

-- 05 Increase readAddress
    Instruction ""      LOAD    $ varAddress varMap                 "spiReadAddress",
    Instruction ""      ADD     $ varAddress (fromRom romVarMap)    "one",
    Instruction ""      STORE   $ varAddress varMap                 "spiReadAddress",

-- 06 ReadAddress > finalAddress ? Jump to finish
    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "finalAddress",
    Instruction ""      SUB     $ varAddress varMap                 "spiReadAddress",
    Instruction ""      BGZ     $ toLabel  mcu6000test              "spir",

    Instruction ""      JMP $ fromIntegral (length mcu6000test - 1)
    ]


main :: IO ()
main = writeAssembledToFile (assembleRom mcu6000test romVarMap) "mcu6000_test.hex"