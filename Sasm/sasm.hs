import Data.Bits ( shiftL, (.|.) )
import Data.Binary ( Word16, encode )
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put (runPut, putWord16le)

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show, Eq)

data Line = Line String Instruction Word16
            deriving (Show)

labelAddress label (line:progs) idx
    | labelIn line == label = idx
    | otherwise = labelAddress label progs (idx + 1)
        where labelIn (Line l _ _) = l
labelAddress _ [] _ = -1

assembleLine :: Line -> Word16
assembleLine (Line l i o) =  shiftL (instToOpCode i) 12 .|. o where
    instToOpCode i = fromIntegral (fromEnum i + 1) :: Word16

writeProgToFile :: [Line] -> FilePath -> IO ()
writeProgToFile p f = B.writeFile f $ runPut $ mapM_ putWord16le (assemble p) where
    assemble = map assembleLine

toLabel l = labelAddress l program 0

---- Assembly Program Begin ----

var_n1 = 0x0499
var_n2 = 0x0498

program :: [Line]
program = [
    Line "Init"     READ var_n1,
    Line ""         READ var_n2,
    Line ""         LOAD var_n1,
    Line ""         SUB var_n2,
    Line ""         BLZ (toLabel "n2w"),
    Line ""         WRITE var_n1,
    Line ""         JMP (toLabel "end"),
    Line "n2w"      WRITE var_n2,
    Line "end"      HALT 0
    ]