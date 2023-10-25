import Sasm

romVarMap :: StaticVarMap
romVarMap = [
        StaticVar (Var "spi_shift" $ romStaticAddr + 0) 0x05,
        StaticVar (Var "spi_prescalar" $ romStaticAddr + 1) 0x04,
        StaticVar (Var "spi_cmd" $ romStaticAddr + 2) 0x01
    ]

varMap :: VarMap
varMap = []

mcu6000test :: Program
mcu6000test = [
    Instruction "init"  LOAD    $ varAddress (fromRom romVarMap)    "spi_shift",
    Instruction ""      STORE   $ varAddress registerMap            "spiShift" ,

    Instruction ""      LOAD    $ varAddress (fromRom romVarMap)    "spi_prescalar",
    Instruction ""      STORE   $ varAddress registerMap            "spiPrescalar" ,

    Instruction "init"  LOAD    $ varAddress (fromRom romVarMap)    "spi_cmd",
    Instruction ""      STORE   $ varAddress registerMap            "spiCmd",

    Instruction ""             JMP $ fromIntegral (length mcu6000test - 1)
    ]

