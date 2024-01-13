module Sasm where
import Data.Bits ( shiftL, (.|.) )
import Data.Binary ( Word16, encode )
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put (runPut, putWord16le)
import Data.List ( find, elemIndex )
import Data.Maybe ( fromJust )

type Address = Word16
type VarName = String
type VarValue = Word16
type Program = [Instruction]
type VarMap  = [Var]
type StaticVarMap = [StaticVar]
type AssebledArray = [Word16]
type Register = (String, Address)
type RegisterMap = [Register]

data OpCode = SSHL | SSHR | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BGZ | BEZ | SNOT | SOR | SAND | PDBG
                  deriving (Enum, Show, Eq)

data Instruction =  Instruction String OpCode Address
            deriving (Show, Eq)

data Var = Var VarName Address deriving (Show, Eq)

data StaticVar = StaticVar VarName VarValue Address deriving (Show, Eq)

assembleProgram ::  Program -> AssebledArray
assembleProgram = map assembleInstruction
    where assembleInstruction (Instruction l i o) =  shiftL (instToOpCode i) 12 .|. o
          instToOpCode i = fromIntegral (fromEnum i + 1) :: Word16

assembleRomStatic :: StaticVarMap -> AssebledArray
assembleRomStatic = map assembleStaticVar
    where assembleStaticVar (StaticVar n v a) = v :: Word16

assembleRom :: Program -> StaticVarMap -> AssebledArray
assembleRom rvm p = assembleProgram rvm ++ fillGap ++ assembleRomStatic p
    where fillGap = replicate numOfWords 0
          numOfWords = wRomStaticAddr - progLen
          progLen = length $ assembleProgram rvm
          wRomStaticAddr = fromInteger (toInteger romStaticAddr)

writeAssembledToFile :: AssebledArray -> FilePath -> IO ()
writeAssembledToFile a f = B.writeFile f $ runPut $ mapM_ putWord16le a

-- ASM Keywords

resolveRomAddr :: [Address -> StaticVar] -> [StaticVar]
resolveRomAddr m = zipWith ($) m (take  (length m) (map (+romStaticAddr) [0,1..]))

resolveRamAddr:: [Address -> Var] -> [Var]
resolveRamAddr m = zipWith ($) m (take  (length m) (map (+ramStartAddr) [0,1..]))


toLabel :: Program -> String -> Word16
toLabel p l = findAddressOf (lineWithLabel l) p :: Word16
    where
        findAddressOf l p = fromIntegral (fromJust $ elemIndex l p) :: Word16
        lineWithLabel lbl = fromJust $ find (\(Instruction l _ _) -> l == lbl) p

varAddress :: VarMap -> VarName -> Address
varAddress vm n = addressOf $ variableWithName n
    where
        addressOf (Var name a) = a
        variableWithName n = fromJust $ find (\(Var name a) -> name == n) vm

getVar :: [StaticVar] -> [Var]
getVar = map (\(StaticVar n v a) -> Var n a)

fromRegMap :: p -> p
fromRegMap x = x

valFromAddressOf :: VarName -> VarMap -> Address
valFromAddressOf n m = varAddress m n

valToAddressOf ::  VarName -> VarMap -> Address
valToAddressOf n m = varAddress m n

-- Memory addresses definitions

romStaticAddr :: Address
romStaticAddr = 0x300

ramStartAddr :: Address
ramStartAddr = 0x400

regMapStartAddr :: Address
regMapStartAddr = 0xF00

registerMap :: VarMap 
registerMap = [
    Var "timerState"     0xF10,
    Var "timerPrescalar" 0xF11,
    Var "timerModulus"   0xF12,
    Var "timerCountVal"  0xF13,
    Var "pwmState"       0xF20,
    Var "pwmWidth"       0xF21,
    Var "spiState"       0xF30,
    Var "spiPrescalar"   0xF31,
    Var "spiCommand"     0xF32,
    Var "spiShift"       0xF33
    ]
