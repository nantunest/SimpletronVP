module Sasm where
import Data.Bits ( shiftL, (.|.) )
import Data.Binary ( Word16, encode )
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put (runPut, putWord16le)
import Data.List ( find, elemIndex )
import Data.Maybe ( fromJust )

type Address = Word16
type VarName = String
type Program = [Line]
type VarMap  = [Var]
type RomVarMap = [RomVar]
type AssebledArray = [Word16]

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show, Eq)

data Line = Line String Instruction Address
            deriving (Show, Eq)

data Var = Var VarName Address deriving (Show, Eq)

data RomVar = RomVar Var Word16 deriving (Show, Eq)

assembleLine :: Line -> Word16
assembleLine (Line l i o) =  shiftL (instToOpCode i) 12 .|. o
    where instToOpCode i = fromIntegral (fromEnum i + 1) :: Word16

assembleProgram ::  Program -> AssebledArray
assembleProgram = map assembleLine

assembleRomVar :: RomVar -> Word16
assembleRomVar (RomVar v w) = w

assembleRom :: RomVarMap -> AssebledArray
assembleRom = map assembleRomVar

writeAssembledToFile :: AssebledArray -> FilePath -> IO ()
writeAssembledToFile a f = B.writeFile f $ runPut $ mapM_ putWord16le a

toLabel :: Program -> String -> Word16
toLabel p l = findAddressOf (lineWithLabel l) p :: Word16
    where
        findAddressOf l p = fromIntegral (fromJust $ elemIndex l p) :: Word16
        lineWithLabel lbl = fromJust $ find (\(Line l _ _) -> l == lbl) p

varAddress :: VarMap -> VarName -> Address
varAddress vm n = addressOf $ variableWithName n
    where
        addressOf (Var name a) = a
        variableWithName n = fromJust $ find (\(Var name a) -> name == n) vm 

fromRom :: [RomVar] -> [Var]
fromRom r = map (\(RomVar var val) -> var) r

ramStartAddr :: Address
ramStartAddr = 0x500

romStartAddr :: Address
romStartAddr = 0x000

romStaticAddr :: Address
romStaticAddr = 0x400

pwmStatusReg :: Address
pwmStatusReg = 0xF20

pwmWidthReg :: Address
pwmWidthReg = 0xF21

timerStatusReg :: Address
timerStatusReg = 0x0F10

timerPrescalarReg :: Address
timerPrescalarReg = 0x0F11

timerModulusReg :: Address
timerModulusReg = 0x0F12

timerCountVal :: Address
timerCountVal = 0x0F13