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

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show, Eq)

data Line = Line String Instruction Address
            deriving (Show, Eq)

data Var = Var VarName Address deriving (Show, Eq)

data RomVar = RomVar Var Word16 deriving (Show, Eq)

assembleLine :: Line -> Word16
assembleLine (Line l i o) =  shiftL (instToOpCode i) 12 .|. o
    where instToOpCode i = fromIntegral (fromEnum i + 1) :: Word16

writeProgToFile :: Program -> FilePath -> IO ()
writeProgToFile p f = B.writeFile f $ runPut $ mapM_ (putWord16le . assembleLine) p

toLabel :: Program -> String -> Word16
toLabel p l = findAddressOf (lineWithLabel l) p :: Word16
    where
        findAddressOf l p = fromIntegral (fromJust $ elemIndex l p) :: Word16
        lineWithLabel lbl = fromJust $ find (\(Line l _ _) -> l == lbl) p

--declareVar :: VarName -> Var
--declareVar name = Var name $ ramStartAddr + (\(Var n a) -> a) (last varMap)

varAddress :: VarMap -> VarName -> Address
varAddress vm n = addressOf $ variableWithName n
    where
        addressOf (Var name a) = a
        variableWithName n = fromJust $ find (\(Var name a) -> name == n) vm 

--nextAddress :: Address
--nextAddress = fromIntegral (length varMap) + ramStartAddr + 1

--- Assembly Program Begin ----
