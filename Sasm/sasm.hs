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

declareVar :: VarName -> Var
declareVar name = Var name $ ramStartAddr + (\(Var n a) -> a) (last varMap)

varAddress :: VarName -> Address
varAddress n = addressOf $ variableWithName n
    where
        addressOf (Var name a) = a
        variableWithName n = fromJust $ find (\(Var name a) -> name == n) varMap

nextAddress :: Address
nextAddress = fromIntegral (length varMap) + ramStartAddr + 1

--- Assembly Program Begin ----
romVarMap :: [Var]
romVarMap = [

    ]

varMap :: [Var]
varMap = [
    Var "t_modulus"     $ ramStartAddr - 1,
    Var "t_mode"        $ ramStartAddr - 2,
    Var "pwm_width"     $ ramStartAddr - 3,
    Var "pwm_mode"      $ ramStartAddr - 4
    ]

ramStartAddr :: Address
ramStartAddr = 0x0500

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


pwmSetProg :: [Line]
pwmSetProg = [
    Line "pwmCofig"     READ $ varAddress "t_modulus",
    Line ""             READ $ varAddress "t_mode",
    Line ""             READ $ varAddress "pwm_width",
    Line ""             READ $ varAddress "pwm_mode",

    Line "pwmSet"       LOAD $ varAddress "t_modulus",
    Line ""             STORE timerModulusReg,

    Line ""             LOAD $ varAddress "pwm_width",
    Line ""             STORE pwmWidthReg,

    Line ""             LOAD $ varAddress "pwm_mode",
    Line ""             STORE pwmStatusReg,

    Line ""             LOAD $ varAddress "t_mode",
    Line ""             STORE timerStatusReg,

    Line ""             JMP $ fromIntegral (length pwmSetProg - 1)
    ]

program = pwmSetProg

main :: IO ()
main = writeProgToFile program "prog.hex"
