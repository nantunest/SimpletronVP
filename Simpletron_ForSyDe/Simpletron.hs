module Simpletron where

import Data.Bits
import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.Memory
import Sasm
import Gimbal

-- ### Operators ###

etoi :: (Enum a) => a -> Integer
etoi = toInteger.fromEnum

itoe :: (Enum a) => Integer -> a
itoe = toEnum.fromInteger

-- ### ForSyDe Extension ###

placedMemorySY :: Int -> Int -> Signal (Access a) -> Signal (AbstExt a)
placedMemorySY place size = mealySY ns o (newMem size)
  where
    ns mem (Read x)    = memState mem (Read (x-place))
    ns mem (Write x v) = memState mem (Write (x-place) v)
    o  mem (Read x)    = memOutput mem (Read (x-place))
    o  mem (Write x v) = memOutput mem (Write (x-place) v)

data SimpFSM            = FETCH | DECODE | EXEC | WRITEBACK
                  deriving (Enum, Show, Eq)

-- ### -------------------- Simpletron Core -------------------------- ###

type SimpPC             = Integer
type SimpAccumulator    = Integer
type SimpInstruction    = Integer
type SimpOpcode         = OpCode
type SimpOperand        = Integer
type SimpDecoded        = (SimpOpcode, SimpOperand)
type SimpState          = (SimpFSM, SimpPC, SimpAccumulator, SimpOpcode, SimpOperand)
type SimpAddress        = Integer
type SimpData           = Integer

type SimpDataInput      = AbstExt SimpData
type SimpDataAccess     = Access SimpData

decode :: SimpInstruction -> (SimpOpcode, SimpOperand)
decode i = (toEnum $ fromInteger (i `shiftR` 12), i .&. 0xFFF)

abstToInt :: SimpDataInput -> Int
abstToInt = fromInteger . fromAbstExt 0

execute :: SimpState -> SimpDataInput -> SimpDataAccess -> (SimpPC, SimpAccumulator, SimpDataAccess)
execute (EXEC, pc, acc, LOAD, op)   din dacc = (pc, fromAbstExt 0 din,           Read (fromIntegral op))
execute (EXEC, pc, acc, STORE, op)  din dacc = (pc, acc,                         Write (fromIntegral op) acc)
execute (EXEC, pc, acc, ADD, op)    din dacc = (pc, acc + fromAbstExt 0 din,     dacc)
execute (EXEC, pc, acc, SUB, op)    din dacc = (pc, acc - fromAbstExt 0 din,     dacc)
execute (EXEC, pc, acc, SSHL, op)   din dacc = (pc, acc `shiftL` abstToInt din,  dacc)
execute (EXEC, pc, acc, SSHR, op)   din dacc = (pc, acc `shiftR` abstToInt din,  dacc)
execute (EXEC, pc, acc, SNOP, op)   din dacc = (pc, acc,                         dacc)
execute (EXEC, pc, acc, PDBG, op)   din dacc = (pc, acc,                         dacc)
execute (EXEC, pc, acc, DIV, op)   din dacc = (pc, acc `div` toInteger (abstToInt din),   dacc)
execute (EXEC, pc, acc, MUL, op)   din dacc = (pc, acc * fromAbstExt 0 din,   dacc)
execute (EXEC, pc, acc, SNOT, op)   din dacc = (pc, complement acc, dacc)
execute (EXEC, pc, acc, SOR, op)   din dacc = (pc, acc .|. fromAbstExt 0 din, dacc)
execute (EXEC, pc, acc, SAND, op)   din dacc = (pc, acc .&. fromAbstExt 0 din, dacc)
execute (EXEC, pc, acc, JMP, op)   din dacc = (op, acc, dacc)
execute (EXEC, pc, acc, BGZ, op)   din dacc = (tBranch acc, acc, dacc)
    where tBranch a
            | a > 0 = op | otherwise = pc
execute (EXEC, pc, acc, BEZ, op) din dacc = (tBranch acc, acc, dacc)
    where tBranch a
            | a == 0 = op | otherwise = pc

simp :: (SimpState, SimpDataAccess) -> Integer -> SimpDataInput -> (SimpState, SimpDataAccess)
simp  ((FETCH, pc, acc, opcode, operand), da) _ d = ((DECODE, pc + 1, acc, opcode, operand), Read $ fromIntegral pc)
simp  ((DECODE, pc, acc, opcode, operand), da) _ d = ((EXEC, pc, acc, nextOpcode, nextOperand), nextDataAccess)
                                                    where
                                                        nextOpcode = fst $ decode (fromAbstExt 0 d)
                                                        nextOperand = snd $ decode (fromAbstExt 0 d)
                                                        nextDataAccess =  Read $ fromIntegral $ snd $ decode (fromAbstExt 0 d)

simp  (s@(EXEC, pc, acc, opcode, operand), da) _ d = ((FETCH, execPc, execAcc, opcode, operand), execDout)
                                                   where (execPc, execAcc, execDout) = execute s d da

simpMP :: Signal Integer -> Signal SimpDataInput -> Signal (SimpState, SimpDataAccess)
simpMP = moore2SY simp outf (s0, Read 0)
    where outf s = s

s0 :: (SimpFSM, SimpPC, SimpAccumulator, OpCode, SimpOperand)
s0 = (FETCH, 0, 0, SNOP, 0)

simpWithMemoryP :: Signal Integer -> Signal (SimpState, SimpDataAccess)
simpWithMemoryP clk = stateOut
    where
        stateOut = simpMP clk (dropS (length progCompiled) sFromMemory)
        sFromMemory = memorySY (length progCompiled) (signal romWriteSig +-+ sndSY stateOut)


-- ### ------------------------------------ Timer ----------------------------------------------- ###

data TimerStatus        = T_INIT | T_COUNT | T_CONTINUE
    deriving (Enum, Show, Eq)

type TimerPrescalar     = Integer
type TimerModulus       = Integer
type TimerCount         = Integer
type TimerTick          = Bool
type TimerRequest       = Bool
type TimerState         = (TimerStatus, TimerPrescalar, TimerModulus, TimerCount, TimerTick)

timer :: TimerState -> SimpDataAccess -> TimerState
timer (T_COUNT,p,m,c,t) _
        | c == m-1    = (T_INIT, p, m, c, True)
        | otherwise = (T_COUNT, p, m, c+1, False)
timer (T_CONTINUE,p,m,c,t) _
        | c == m-1    = (T_CONTINUE, p, m, 0, True)
        | otherwise = (T_CONTINUE, p, m, c+1, False)
timer (T_INIT,p,m,c,t) (Write 0xF10 d) = (itoe d, p, m, 0, False)
timer (T_INIT,p,m,c,t) (Write 0xF11 d) = (T_INIT, d, m, c, False)
timer (T_INIT,p,m,c,t) (Write 0xF12 d) = (T_INIT, p, d , c, False)
timer (s,   p,m,c,t) (Read 0xF13) = (s, p, m, c, False)
timer s _ = s

-- If more than one signal in, use mooreSY2,3
timerP :: Signal SimpDataAccess -> Signal (TimerState, SimpDataInput)
timerP = mealySY timer outf (T_INIT,1,0,0,False)
    where
        outf st@(_,_,_,c,_) (Read 0xF13) = (st, Prst c)
        outf st@(_,_,_,c,_) dacc         = (st, Abst)

joinSimpDataRead :: SimpDataInput -> SimpDataInput -> SimpDataInput
joinSimpDataRead Abst (Prst v) = Prst v
joinSimpDataRead (Prst v) Abst = Prst v
joinSimpDataRead Abst Abst = Abst
joinSimpDataRead (Prst v1) (Prst v2) = error $ "Two values present." ++ show v1  ++ " " ++ show v2

joinSimpDataReadP :: Signal SimpDataInput -> Signal SimpDataInput -> Signal SimpDataInput
joinSimpDataReadP = zipWithSY joinSimpDataRead

simpProcNetP :: Signal Integer -> Signal ((SimpState, SimpDataAccess), SimpDataInput)
simpProcNetP clk = zipWithSY (,) stateOut sDataRead
    where
        stateOut = simpMP clk sDataRead
        sDataAccess = sndSY stateOut
        sDataRead = muxP sDataAccess

-- ### MUX ###
muxP :: Signal SimpDataAccess -> Signal SimpDataInput
muxP dacc = sOut
    where
        sOut = xSpi $ xPwm $ xTimer sRomxRam
        sRomxRam = joinSimpDataReadP (dropS (length progCompiled) $ romMemoryAccess dacc) (ramMemoryAccess dacc)
        sTimer = timerP dacc
        xTimer = joinSimpDataReadP $ sndSY sTimer
        sPwm = pwmP dacc $ fstSY sTimer
        xPwm = joinSimpDataReadP $ sndSY sPwm
        sSpi = spiWithMpu6000 dacc
        xSpi = joinSimpDataReadP $ (sndSY.fstSY) sSpi

-- ### PWM ###

data PwmStatus = P_INIT | P_RUNNING
    deriving (Enum, Show, Eq)
type PwmWidth = Integer
type PwmOut = Bool
type PwmState = (PwmStatus, PwmWidth, PwmOut)

pwm :: PwmState -> SimpDataAccess -> TimerState -> PwmState
pwm ps@(P_RUNNING, w,_) _ (_,_,_,c,_) = (P_RUNNING, w, c < w)
pwm ps@(P_INIT, w,_) (Write 0xF20 d) t
    | (toEnum . fromInteger) d == P_INIT = (P_INIT, w, False)
    | otherwise = (P_RUNNING, w, True)
pwm ps@(P_INIT, w,_) (Write 0xF21 d) t = (P_INIT, d, False)
pwm ps _ _ = ps

pwmP :: Signal SimpDataAccess -> Signal TimerState -> Signal (PwmState, SimpDataInput)
pwmP = mealy2SY pwm outf (P_INIT, 0, False)
    where outf ps _ _ = (ps, Abst)

pwmWithTimer :: Signal SimpDataAccess -> Signal PwmState
pwmWithTimer da = fstSY pwmOut
    where
        pwmOut = pwmP da tc
        tc = fstSY $ timerP da

-- ### SPI ###

data SpiStatus = S_INIT | S_READY | S_SHIF | S_DONE
    deriving (Enum, Show, Eq)

type SpiSclk = Bool
type SpiMiso = Bool
type SpiMosi = Bool
type SpiSS = Bool
type SpiPrescalar = Integer
type SpiShift = Integer
type SpiShiftCounter = Integer
type SpiState = (SpiStatus, SpiSclk, SpiMosi, SpiSS, SpiPrescalar, SpiShift, SpiShiftCounter)

spi :: SpiState -> SimpDataAccess -> SpiMiso -> SpiState
spi s@(S_SHIF, sclk, mo, ss, p, spiShift, c) _ mi
    | c == 16 = (S_DONE, False, False, False, p, nextShift, 0)
    | otherwise = (S_SHIF, False, nextMo, True, p, nextShift, c+1)
        where
            nextShift = (etoi mi `shiftL` 15) .|. (spiShift `shiftR` 1)
            nextMo = itoe (spiShift .&. 0x0001)

spi s@(S_READY, sclk, mo, ss, p, spiShift,c) _ _ = (S_SHIF, True, mo, False, p, spiShift, 0)
spi s@(S_INIT, sclk, mo, ss, p, spiShift,c) (Write 0xF30 d) _ = (itoe d, sclk, mo, False, p, spiShift,c)    -- status
spi s@(S_INIT, sclk, mo, ss, p, spiShift,c) (Write 0xF31 d) _ = (S_INIT, sclk, mo, False, d, spiShift, c)   -- prescalar
spi s@(S_INIT, sclk, mo, ss, p, spiShift,c) (Write 0xF32 1) _ = (S_READY, sclk, mo, False, p, spiShift,c)   -- command
spi s@(S_INIT, sclk, mo, ss, p, spiShift,c) (Write 0xF33 d) _ = (S_INIT, sclk, mo, False, p, d, c)          -- shift
spi s _ _ = s

spiP :: Signal SimpDataAccess -> Signal SpiMiso -> Signal (SpiState, SimpDataInput)
spiP = mealy2SY spi outf (S_INIT, False, False, False, 1, 0, 0)
    where
        outf ps@(fsm,_,_,_,_,_,_) (Read 0xF30) _ = (ps, Prst $ etoi fsm)
        outf ps@(fsm,_,_,_,_,r,_) (Read 0xF33) _ = (ps, Prst $ etoi r)
        outf ps _ _ = (ps, Abst)

data SpiDevStatus = IDLE | SHIF
    deriving (Enum, Show, Eq)
type SpiDevState = (SpiDevStatus, SpiSclk, SpiMiso, SpiShift, SpiShiftCounter)
type SpiDevState2 = (SpiDevStatus, SpiSclk, SpiMiso, SpiShift, SpiShiftCounter, Int)

spiDev :: SpiDevState -> SpiSS -> SpiMosi -> SpiDevState
spiDev (IDLE, sclk, mi, devShift, c) True mo = (SHIF, sclk, mi, devShift, 0)
spiDev (SHIF, sclk, mi, devShift, c) True mo
    | c == 17 = (IDLE, sclk, mi, devShift, 0)
    | otherwise = (SHIF, sclk, nextMi, nextShift, c+1)
        where
            nextShift = (etoi mo `shiftL` 15) .|. (devShift `shiftR` 1)
            nextMi = itoe (devShift .&. 0x0001)
spiDev (SHIF, sclk, mi, devShift, c) False _ = (IDLE, sclk, mi, devShift, c)
spiDev s _ _ = s

spiDevP :: Signal SpiSS -> Signal SpiMosi -> Signal SpiDevState
spiDevP = moore2SY spiDev outf (IDLE, False, False, 0x00, 0)
    where outf ps = ps

gyroXoutS :: [Integer]
gyroXoutS = [1,1,2,3,5,8,13,21,0,0,0,0,0,0,0,0,0,0,0,0,0,38,0,-9,-8,-5,-4,-3,-2,-2,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-38,0,9,8,5,4,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38,0,-9,-8,-5,-4,-3,-2,-2,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-39,0,10,8,5,4,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
baseAddr :: Integer
baseAddr = 0x3B

mpu6000 :: SpiDevState2 -> SpiSS -> SpiMosi -> SpiDevState2
mpu6000 (s, sclk, mi, devShift, c, p) True mo = (s, sclk, nextMi, nextShift, c+1, pNext)
        where
            nextMi = itoe (nextShift .&. 0x01)
            nextShift
                | (c `mod` 8 == 0) && (c /= 0) = replaceShift
                | otherwise = (etoi mo `shiftL` 7) .|. (devShift `shiftR` 1)
            replaceShift
                | devShift-baseAddr == 8 = (gyroXoutS!!p) `shiftR` 8
                | devShift-baseAddr == 9 = (gyroXoutS!!p) .&. 0xFF
                | otherwise = error $ "reg is " ++ show (devShift-baseAddr)
            pNext
                | devShift-baseAddr == 9 = p+1
                | otherwise = p
mpu6000 s _ _ = s

mpu6000P :: Signal SpiSS -> Signal SpiMosi -> Signal SpiDevState2
mpu6000P = moore2SY mpu6000 outf (IDLE, False, False, 0x00, 0, 0)
    where outf ps = ps

getSpiDevMiso :: SpiDevState2 -> SpiMosi
getSpiDevMiso (_,_,mi,_,_,_) = mi

-- type SpiState = (SpiStatus, SpiSclk, SpiMosi, SpiSS, SpiPrescalar, SpiShift, SpiShiftCounter)
getSpiSS :: SpiState -> SpiSS
getSpiSS (_,_,_,ss,_,_,_) = ss

getSpiMosi:: SpiState -> SpiSS
getSpiMosi (_,_,mo,_,_,_,_) = mo

getSimpDataInput :: ((SpiState, SimpDataInput), SpiDevState2) -> SimpDataInput
getSimpDataInput ( (_,sin),_) = sin

spiWithMpu6000 :: Signal SimpDataAccess -> Signal ((SpiState, SimpDataInput),SpiDevState2)
spiWithMpu6000 dacc = zipWithSY (,) sOut sMPU6000
    where
        sOut = sSpi
        sSpi = spiP dacc sMiso
        sMiso = mapSY getSpiDevMiso sMPU6000 
        sMPU6000 = mpu6000P sSS sMosi
        sSS = mapSY getSpiSS $ fstSY sSpi
        sMosi = mapSY getSpiMosi $ fstSY sSpi

-- ### Program ###

sInstructions :: Signal Sasm.Instruction
sInstructions = signal Gimbal.gimbal

progCompiled :: AssebledArray
progCompiled = assembleRom Gimbal.gimbal romVarDecl

romWriteSig :: [Access Integer]
romWriteSig = [Write a b | (a,b) <- zip (take (length progCompiled) [0..]) (map fromIntegral progCompiled)]

romMemoryAccess :: Signal SimpDataAccess -> Signal SimpDataInput
romMemoryAccess s
    | isAccessValid (headS s) = placedMemorySY 0x000 0x400 (signal romWriteSig +-+ s)
    | otherwise = error "Cannot write in ROM memory."
        where
            isAccessValid (Read a) = True
            isAccessValid (Write a _)
                | a >= 0x400 = True
                | otherwise = False

ramMemoryAccess :: Signal SimpDataAccess -> Signal SimpDataInput
ramMemoryAccess = placedMemorySY 0x400 0xB00

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

st0 :: (TimerStatus, Integer, Integer, Integer, Bool, Bool)
st0 = (T_INIT,1,0,0,True,False)

testProg :: Program
testProg = [
    Instruction "" LOAD  $ valFromAddressOf "myVar1" $ getVar romVarDecl2,
    Instruction "" ADD   $ valFromAddressOf "myVar2" $ getVar romVarDecl2,
    Instruction "" STORE $ valToAddressOf "result"  ramVarDecl2,
    Instruction "" LOAD  $ valFromAddressOf "myVar1" $ getVar romVarDecl2,
    Instruction "" LOAD  $ valToAddressOf "result" ramVarDecl2
    ]

spiPrettyPrint :: Signal (SpiState, SpiDevState) -> String
spiPrettyPrint s = header ++ concatMap unpackSpiS (fromSignal s)
    where
        header = "SpiStatus\tSpiSclk\tSpiMosi\tSpiSS\tSpiPrescalar\tSpiShift\tSpiShiftCounter\n"
        unpackSpiS (s1, s2) = printS1 s1 ++ "\n" -- ++ printS2 s2 ++ "\n"
        printS1 (a,b,c,d,e,f,g) = show a ++ "\t\t" ++ show b ++ "\t" ++
                                  show c ++ "\t" ++ show d ++ "\t" ++
                                  show e ++ "\t\t\t\t" ++ show f ++ "\t\t\t" ++
                                  show g

spiDevPrettyPrint :: Signal (SpiState, SpiDevState) -> String
spiDevPrettyPrint s = header ++ concatMap unpackSpiS (fromSignal s)
    where
        header = "SpiDevStatus\tSpiSclk\tSpiMiso\tSpiShift\tSpiShiftCounter\n"
        unpackSpiS (s1, s2) = printS2 s2 ++ "\n" -- ++ printS2 s2 ++ "\n"
        printS2 (a,b,c,d,e)     = show a ++ "\t\t\t" ++ show b ++ "\t" ++
                                  show c ++ "\t" ++ show d ++ "\t\t\t" ++
                                  show e

simpExecute :: Integer -> Signal ((SimpState, SimpDataAccess), SimpDataInput)
simpExecute a = simpProcNetP $ signal [0..a]

filterPDBG :: ((SimpState, SimpDataAccess), SimpDataInput) -> AbstExt (SimpOpcode, SimpAccumulator)
filterPDBG (((EXEC,_,a,PDBG,_),dacc),inp) = Prst (PDBG, a)
filterPDBG _ = Abst

main :: IO ()
main = putStr $ show $ simpExecute 10000

addrh =0x3B+8
addrl =0x3B+9
--mapSY getSimpDataInput $ spiWithMpu6000 $ signal $ [Write 0xF33 addrh, Write 0xF33 1] ++ replicate 30 (Read 0xF30)