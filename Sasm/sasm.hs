import Text.Printf

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show)

data Line = Line Int Instruction Int String
            deriving (Show)

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

assembleLine :: Line -> String
assembleLine (Line n i o l) = "0x" ++ show (instToOpCode i) ++ printf "%x" o

assemble :: [Line] -> [String]
--assemble (l:ls) = show l : assemble ls
--assemble [] = []
assemble = map assembleLine

var_n1 = 0x0499
var_n2 = 0x0498

program :: [Line]
program = [
    Line 00 READ var_n1 "init",
    Line 01 READ var_n2 "",
    Line 03 LOAD var_n1 ""
    ]
