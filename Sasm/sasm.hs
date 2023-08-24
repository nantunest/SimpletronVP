{-# LANGUAGE ViewPatterns #-}
import Text.Printf
import Data.Maybe (fromMaybe)

data Instruction = READ | WRITE | LOAD | STORE | ADD | SUB | DIV | MUL | JMP | BLZ | BEZ | HALT
                  deriving (Enum, Show)

label l
    | l == "None" = Nothing
    | otherwise = Just l

--line n l i o = putStrLn $ show n ++ show l ++ show i ++ show o
line :: (PrintfType t4) => Int -> Maybe String -> Instruction -> Int -> t4
line n (fromMaybe "" -> l) i = printf "%d %s %s 0x%x\n" n  l (show i)

var_n1 = 0x0499
var_n2 = 0x0498

program :: PrintfType a => [a]
program = [
    line 00 (label "init") READ var_n1,
    line 01 (label "None") READ var_n1
    ]

prog :: PrintfType a => a
prog = line 0 (label "init") READ var_n1