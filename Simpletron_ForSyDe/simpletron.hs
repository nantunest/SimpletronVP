import Data.Bits
import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.Memory
import Sasm
import Gimbal

data SimpFSM            = FETCH | DECODE | EXEC | WRITEBACK
                  deriving (Enum, Show, Eq)

type SimpPC             = Integer
type SimpAccumulator    = Integer
type SimpInstruction    = Integer
type SimpOpcode         = OpCode
type SimpOperand        = Integer
type SimpDecoded        = (SimpOpcode, SimpOperand)
type SimpState          = (SimpFSM, SimpPC, SimpAccumulator, SimpOpcode, SimpOperand, SimpDataAccess)
type SimpAddress        = Integer
type SimpData           = Integer

type SimpDataInput       = AbstExt SimpData
type SimpDataAccess     = Access SimpData

-- ### Simpletron Core ###

decode :: SimpInstruction -> (SimpOpcode, SimpOperand)
decode i = (toEnum $ fromInteger (i `shiftR` 12), i .&. 0xFFF)

-- (SimpFSM, SimpPC, SimpAccumulator, SimpOpcode, SimpOperand, SimpDataOut, AddressOut)
execute :: SimpOpcode -> SimpAccumulator -> SimpOperand -> SimpDataInput -> (SimpAccumulator, SimpDataAccess)
execute LOAD    acc op din = (fromAbstExt 0 din,            Read (fromIntegral op))
execute STORE   acc op din = (acc,                          Write (fromIntegral op) acc)
execute ADD     acc op din = (acc + fromAbstExt 0 din,      Read (fromIntegral op))
execute SUB     acc op din = (acc - fromAbstExt 0 din,      Read (fromIntegral op))
execute SSHL    acc op din = (acc `shiftL` fromInteger op,  Read 0)
execute SSHR    acc op din = (acc `shiftR` fromInteger op,  Read 0)
execute SNOP    acc op din = (acc,                          Read 0)
execute PDBG    acc op din = (acc,                          Read 0)

simp :: SimpState  -> Integer -> SimpDataInput -> SimpState
simp  (EXEC, pc, acc, opcode, operand, dout) _ d = (FETCH, pc, execAcc, opcode, operand, execDout)
                                                   where
                                                       (execAcc, execDout) = execute opcode acc operand d
simp  (DECODE, pc, acc, opcode, operand, dout) _ d = (EXEC, pc, acc, fst $ decode (fromAbstExt 0 d), snd $ decode (fromAbstExt 0 d), Read $ fromIntegral $ snd $ decode (fromAbstExt 0 d))
simp  (FETCH, pc, acc, opcode, operand, dout) _ d = (DECODE, pc + 1, acc, opcode, operand, Read $ fromIntegral pc)

--simpP :: Signal SimpState -> Signal SimpDataRead -> Signal SimpState
--simpP = zipWithSY simp

-- timerP :: Signal SimpDataAccess -> Signal (TimerState, SimpDataRead)
-- timerP = mooreSY timer outf (INIT,1,0,0,False,False)

simpMP :: Signal Integer -> Signal SimpDataInput -> Signal (SimpState, SimpDataAccess)
simpMP = moore2SY simp outf s0
    where outf s@(_,_,_,_,_,da) = (s,da)

memOp :: SimpState -> SimpDataAccess
memOp (_,_,_,_,_,op) = op

s0 = (FETCH, 0, 0, SNOP, 0, Read 0)

simpWithMemoryP :: Signal Integer -> Signal (SimpState, SimpDataAccess)
simpWithMemoryP clk = stateOut
    where
        stateOut = simpMP clk (dropS (length progCompiled) sFromMemory)
        sFromMemory = memorySY (length progCompiled) (signal romWriteSig +-+ sndSY stateOut)

-- ### Timer ###

data TimerStatus        = INIT | COUNT | RESET
    deriving (Enum, Show, Eq)

type TimerPrescalar     = Integer
type TimerModulus       = Integer
type TimerCount         = Integer
type TimerTick          = Bool
type TimerRequest       = Bool
type TimerState         = (TimerStatus, TimerPrescalar, TimerModulus, TimerCount, TimerTick, TimerRequest)

timer :: TimerState -> SimpDataAccess -> TimerState
timer (COUNT,p,m,c,t,_) _
        | c == m    = (INIT, p, m, c, True, False)
        | otherwise = (COUNT, p, m, c+1, False, False)
timer (INIT,p,m,c,t,_) (Write 0xF10 d) = ((toEnum . fromInteger) d, p, m, 0, False, False)
timer (INIT,p,m,c,t,_) (Write 0xF11 d) = (INIT, d, m, c, False, False)
timer (INIT,p,m,c,t,_) (Write 0xF12 d) = (INIT, p, d , c, False, False)
timer (s,   p,m,c,t,_) (Read 0xF13) = (s, p, m, c, False, True)
timer s _ = s

-- If more than one signal in, use mooreSY2,3
timerP :: Signal SimpDataAccess -> Signal (TimerState, SimpDataInput)
timerP = mooreSY timer outf (INIT,1,0,0,False,False)
    where
        outf st@(_,_,_,c,_,False) = (st, Abst)
        outf st@(_,_,_,c,_,True) = (st, Prst c)

joinSimpDataRead :: SimpDataInput -> SimpDataInput -> SimpDataInput
joinSimpDataRead Abst (Prst v) = Prst v
joinSimpDataRead (Prst v) Abst = Prst v
joinSimpDataRead Abst Abst = Abst
joinSimpDataRead (Prst v1) (Prst v2) = error "Two values present."

joinSimpDataReadP :: Signal SimpDataInput -> Signal SimpDataInput -> Signal SimpDataInput
joinSimpDataReadP = zipWithSY joinSimpDataRead

simpWithTimerP :: Signal Integer -> Signal (SimpState, SimpDataAccess)
simpWithTimerP clk = stateOut
    where
        stateOut = simpMP clk sDataRead
        sDataAccess = sndSY stateOut
        sToMemToSimp = dropS (length progCompiled) sRomMemory
        sTimer = sndSY $ timerP sDataAccess
        sDataRead = joinSimpDataReadP sTimer sToMemToSimp
        sRomMemory = memorySY (length progCompiled) (signal romWriteSig +-+ sDataAccess)

-- ### Program ###

sInstructions :: Signal Sasm.Instruction
sInstructions = signal Gimbal.gimbal

progCompiled :: AssebledArray
progCompiled = assembleRom testProg romVarDecl2

romWriteSig :: [Access Integer]
romWriteSig = [Write a b | (a,b) <- zip (take (length progCompiled) [0..]) (map fromIntegral progCompiled)]

romMemoryRead :: Signal (Access Integer) -> Signal (AbstExt Integer)
romMemoryRead s = memorySY (length progCompiled) (signal romWriteSig +-+ s)

romVarDecl2 :: [StaticVar]
romVarDecl2 = resolveRomAddr [
        StaticVar "myVar1"  999,
        StaticVar "myVar2"  50,
        StaticVar "tm" 5,
        StaticVar "one" 1
    ]

ramVarDecl2 :: VarMap
ramVarDecl2 = resolveRamAddr [
        Var "result"
    ]

st0 = (INIT,1,0,0,True,False)
-- simpExec :: (Eq t, Num t) => Signal (SimpState,TimerState) -> t -> Signal (SimpState,TimerState)
-- simpExec _ 0 = signal [(s0, st0)]
-- simpExec s c = r +-+ simpExec r (c-1)
--     where
--         r = simpWithTimerP s

testProg :: Program
testProg = [
    Instruction "" LOAD  $ valFromAddressOf "myVar1" $ getVar romVarDecl2,
    Instruction "" ADD   $ valFromAddressOf "myVar2" $ getVar romVarDecl2,
    Instruction "" STORE $ valToAddressOf "result"  ramVarDecl2
    ]