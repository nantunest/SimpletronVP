import Data.Binary ( Word16, encode )
import ForSyDe.Shallow

import ForSyDe.Shallow.Utility.Memory
import Sasm
import Gimbal

sInstructions :: Signal Sasm.Instruction
sInstructions = signal Gimbal.gimbal
progCompiled :: AssebledArray
progCompiled = assembleRom gimbal romVarDecl

romWriteSig :: [Access Integer]
romWriteSig = [Write a b | b <- map fromIntegral progCompiled, a <- [0..]]


romMemory :: Signal (AbstExt Integer)
romMemory = memorySY 10 $ takeS 10 $ signal romWriteSig