import Text.Printf
import Data.Bits ( Bits(shiftR, shiftL, (.|.), (.&.)) )
import Data.Binary ( Word16, encode )
import qualified Data.ByteString.Lazy as B
import GHC.Builtin.PrimOps (PrimOp(Word16AddOp))

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show, Eq)

data Label = Label String Int
            deriving (Show)

data Line = Line String Instruction Int
            deriving (Show)

data ASMKEYS = InProg

--label l
--    | l == "None" = Nothing
--    | otherwise = Just l
--
--line' :: Int -> Instruction -> Int -> String -> String
--line' n i o l = show n ++ " " ++ show i ++ " " ++ show o ++ " " ++ l ++ "\n"
--
--line :: Int -> Instruction -> Int -> String -> (Int, Instruction, Int, String)
--line n i o l = (n, i, o, l)

instToOpCode :: Instruction -> Int
instToOpCode i = fromEnum i + 1

labelIn (Line l _ _) = l
addressIn (Label _ a) = a

-- Finds the label address given a label, a program and a starting address
-- Returns the label address
labelAddress label (line:progs) idx
    | labelIn line == label = idx
    | otherwise = labelAddress label progs (idx + 1)
labelAddress _ [] _ = -1

toLabel l InProg p = labelAddress l p 0

-- Finds the address of all the labels in a program.
-- Returns [Label]
labelAddresses (line:ps) pa
    | labelIn line /= "" = Label (labelIn line) (labelAddress (labelIn line) pa 0) : labelAddresses ps pa
    | otherwise = labelAddresses ps pa
labelAddresses [] _ = []

getLabelAddresses prog = labelAddresses prog prog

assembleLine :: Line -> Int
assembleLine (Line l i o) =  shiftL (instToOpCode i) 12 .|. o
--assembleLine (Line l i o) = "0x" ++ printf "%x" (instToOpCode i) ++ printf "%.3x" o

assemble :: [Line] -> [Int]
assemble = map assembleLine

writeIntsToFile :: FilePath -> [Int] -> IO ()
writeIntsToFile fp is  = B.writeFile fp $ encode (map toWord16 is) where
                            toWord16::Int -> Word16
                            toWord16 x = fromIntegral (((x .&. 0xFF00) `shiftR` 8) .|. ((x .&. 0xFF) `shiftL` 8)) 

var_n1 = 0x0499
var_n2 = 0x0498

program :: [Line]
program = [
    Line "Init"     READ var_n1,
    Line ""         READ var_n2,
    Line ""         LOAD var_n1,
    Line ""         SUB var_n2,
    Line ""         BLZ (toLabel "n2w" InProg program),
    Line ""         WRITE var_n1,
    Line ""         JMP (toLabel "end" InProg program),
    Line "n2w"      WRITE var_n2,
    Line "end"      HALT 0
    ]