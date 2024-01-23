import Data.Bits
import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.Memory
import Sasm
import Gimbal
import Control.Monad.Trans.RWS (state)

data SimpFSM            = FETCH | DECODE | EXEC | WRITEBACK
                  deriving (Enum, Show, Eq)


type SimpMemory         = [Integer]
type SimpPC             = Integer
type SimpAccumulator    = Integer
type SimpInstruction    = Integer
type SimpOpcode         = OpCode
type SimpOperand        = Integer
type SimpDecoded        = (SimpOpcode, SimpOperand)
type SimpState          = (SimpFSM, SimpPC, SimpAccumulator, SimpOpcode, SimpOperand, SimpDataOut)


type TimerStatus        = Integer
type TimerPrescalar     = Integer
type TimerModulus       = Integer
type TimerCount         = Integer
type TimerState         = (TimerStatus, TimerPrescalar, TimerModulus, TimerCount)

type SimpMemorySig      = Signal SimpMemory
type SimpPCSig          = Signal SimpPC
type SimpAccumulatorSig = Signal SimpAccumulator
type SimpInstructionSig = Signal SimpInstruction
type SimpOpcodeSig      = Signal SimpOpcode
type SimpOperandSig     = Signal SimpOperand

type SimpDecodedSig     = Signal SimpDecoded
type SimpStateSig       = Signal SimpState
type SimpStateInSig     = Signal SimpState
type SimpStateOutSig    = Signal SimpState

type SimpAddress        = Integer
type SimpData           = Integer
type SimpDataIn         = AbstExt SimpData
type SimpDataOut        = Access SimpData
type AddressOut         = SimpAddress
type AddressIn          = SimpAddress

type AddressInSig       = Signal SimpAddress
type AddressOutSig      = Signal SimpAddress
type DataInSig          = Signal SimpDataIn
type DataOutSig         = Signal SimpData

sInstructions :: Signal Sasm.Instruction
sInstructions = signal Gimbal.gimbal

romVarDecl2 :: [StaticVar]
romVarDecl2 = resolveRomAddr [
        StaticVar "myVar1"  999,
        StaticVar "myVar2"  50
    ]

ramVarDecl2 :: VarMap
ramVarDecl2 = resolveRamAddr [
        Var "result"
    ]

progCompiled :: AssebledArray
progCompiled = assembleRom testProg romVarDecl2

romWriteSig :: [Access Integer]
romWriteSig = [Write a b | (a,b) <- zip (take (length progCompiled) [0..]) (map fromIntegral progCompiled)]

romMemoryRead :: Signal (Access Integer) -> Signal (AbstExt Integer)
romMemoryRead s = memorySY (length progCompiled) (signal romWriteSig +-+ s)

-- zeroMem = take (round ((2**16 - 1)::Float)) [0,0..]
zeroMem = take (round ((1500 - 1)::Float)) [0,0..]

initRomMem m p = p ++ drop (length p) m

-- Simpletron Core

--fetch :: SimpMemory -> SimpPC -> SimpInstruction
--fetch m pc = m!!fromInteger pc

--fetchP :: SimpMemorySig -> SimpPCSig -> SimpInstructionSig
--fetchP sMem sPc = zipWithSY fetch sMem sPc


decode :: SimpInstruction -> (SimpOpcode, SimpOperand)
decode i = (toEnum $ fromInteger (i `shiftR` 12), i .&. 0xFFF)
-- 
-- decodeP :: SimpInstructionSig -> SimpDecodedSig
-- decodeP i = mapSY decode i

slice begin end = take (end - begin) . drop begin
writeMem a m v = slice 0 a m ++ [m!!a] ++ slice (a + 1) (length m) m

-- (SimpFSM, SimpPC, SimpAccumulator, SimpOpcode, SimpOperand, SimpDataOut, AddressOut)
execute :: SimpOpcode -> SimpAccumulator -> SimpOperand -> SimpDataIn -> (SimpAccumulator, SimpDataOut)
execute LOAD    acc op din = (fromAbstExt 0 din,            Read (fromIntegral op))
execute STORE   acc op din = (acc,                          Write (fromIntegral op) acc)
execute ADD     acc op din = (acc + fromAbstExt 0 din,      Read (fromIntegral op))
execute SUB     acc op din = (acc - fromAbstExt 0 din,      Read (fromIntegral op))
execute SSHL    acc op din = (acc `shiftL` fromInteger op,  Read 0)
execute SSHR    acc op din = (acc `shiftR` fromInteger op,  Read 0)
execute SNOP    acc op din = (acc,                          Read 0)

simp :: SimpState -> SimpDataIn -> SimpState
simp (EXEC, pc, acc, opcode, operand, dout) d = (FETCH, pc, execAcc, opcode, operand, execDout)
                                                    where
                                                        (execAcc, execDout) = execute opcode acc operand d
simp (DECODE, pc, acc, opcode, operand, dout) d = (EXEC, pc, acc, fst $ decode (fromAbstExt 0 d), snd $ decode (fromAbstExt 0 d), Read $ fromIntegral $ snd $ decode (fromAbstExt 0 d))
simp (FETCH, pc, acc, opcode, operand, dout) d = (DECODE, pc + 1, acc, opcode, operand, Read $ fromIntegral pc)

simpP :: SimpStateInSig -> DataInSig -> SimpStateOutSig
simpP s d = zipWithSY simp s d

memOp :: SimpState -> Access SimpData
memOp (_,_,_,_,_,op) = op

s0 :: Signal (SimpFSM, SimpPC, SimpAccumulator, OpCode, SimpOperand, Access a)
s0 = signal [(FETCH, 0, 0, SNOP, 0, Read 0)]

simpWithMemoryP :: SimpStateInSig -> SimpStateOutSig 
simpWithMemoryP stateIn = stateOut
    where 
        stateOut = simpP stateIn (dropS (length progCompiled) sFromMemory)
        sFromMemory = romMemoryRead memOpMap 
        memOpMap = mapSY memOp stateIn

simpExec :: (Eq t, Num t) => SimpStateInSig -> t -> SimpStateOutSig 
simpExec _ 0 = signal []
simpExec s c = r +-+ simpExec r (c-1)
    where
        r = simpWithMemoryP s

testProg :: Program
testProg = [
    Instruction "" LOAD $ valFromAddressOf "myVar1" $ getVar romVarDecl2,
    Instruction "" STORE $ valToAddressOf "result" ramVarDecl2
    ]