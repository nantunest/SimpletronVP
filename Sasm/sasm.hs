{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Bits ( shiftL, (.|.) )
import Data.Binary ( Word16, encode )
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put (runPut, putWord16le)
import Data.List ( find, elemIndex )
import Data.Maybe ( fromJust )

type Address = Word16
type VarName = String

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show, Eq)

data Line = Line String Instruction Address
            deriving (Show, Eq)

data Var = Var VarName Address deriving (Show, Eq)

assembleLine :: Line -> Word16
assembleLine (Line l i o) =  shiftL (instToOpCode i) 12 .|. o
    where instToOpCode i = fromIntegral (fromEnum i + 1) :: Word16

writeProgToFile :: [Line] -> FilePath -> IO ()
writeProgToFile p f = B.writeFile f $ runPut $ mapM_ (putWord16le . assembleLine) p

toLabel :: String -> Word16
toLabel l = findAddressOf (lineWithLabel l) program :: Word16
    where
        findAddressOf l p = fromIntegral (fromJust $ elemIndex l p) :: Word16
        lineWithLabel lbl = fromJust $ find (\(Line l _ _) -> l == lbl) program

declareVar :: VarName -> Address -> [Var] -> [Var]
declareVar name addr vars = vars ++ [Var name addr]

---- Assembly Program Begin ----

var_n1 = 0x0499
var_n2 = 0x0498

ramStartAddr :: Word16
ramStartAddr = 0x0400

pwmDutyCycleAddr :: Word16
pwmDutyCycleAddr = ramStartAddr


pwmSetProg :: [Line]
pwmSetProg = [
    Line "pwmSet"   LOAD    pwmDutyCycleAddr
    ]

program :: [Line]
program = [
    Line "Init"     READ    var_n1,
    Line ""         READ    var_n2,
    Line ""         LOAD    var_n1,
    Line ""         SUB     var_n2,
    Line ""         BLZ     $ toLabel "n2w",
    Line ""         WRITE   var_n1,
    Line ""         JMP     $ toLabel "end",
    Line "n2w"      WRITE   var_n2,
    Line "end"      HALT    0
    ]

main :: IO ()
main = writeProgToFile program "prog.hex"