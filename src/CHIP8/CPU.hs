{-# LANGUAGE RecordWildCards, LambdaCase #-}
module CHIP8.CPU where

import Clash.Prelude

import CHIP8.Types
import CHIP8.Opcode
import CHIP8.ALU

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Barbies
import RetroClash.Stack

import Barbies
import Barbies.Bare
import Data.Barbie.TH

import Data.Monoid (Last(..))

import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, assign)
import Data.Foldable (for_)
import Data.Maybe

import Debug.Trace

declareBareB [d|
  data CPUIn = CPUIn
      { memRead :: Byte
      , vidRead :: VidRow
      , keyState :: KeypadState
      , tick :: Bool
      } |]

declareBareB [d|
  data CPUOut = CPUOut
      { _memAddr :: Addr
      , _memWrite :: Maybe Byte
      , _vidAddr :: VidY
      , _vidWrite :: Maybe VidRow
      } |]
makeLenses ''CPUOut

data Phase
    = Init
    | Fetch
    | Exec Byte
    | StoreReg Reg
    | LoadReg Reg
    | ClearFB VidY
    | DrawRead VidX VidY Nybble
    | DrawWrite VidX VidY Nybble
    | WaitKeyRelease Reg KeypadState
    | WriteBCD Byte (Index 3)
    deriving (Show, Generic, NFDataX)

data CPUState = CPUState
    { _pc, _ptr :: Addr
    , _registers :: Vec 16 Byte
    , _stack :: Stack 24 Addr
    , _phase :: Phase
    , _timer :: Byte
    , _randomState :: Unsigned 9
    }
    deriving (Show, Generic, NFDataX)

initState :: CPUState
initState = CPUState
    { _pc = 0x200
    , _ptr = 0x000
    , _registers = repeat 0
    , _stack = Stack (repeat 0) 0
    , _phase = Init
    , _timer = 0
    , _randomState = 0
    }
makeLenses ''CPUState

defaultOut :: CPUState -> Pure CPUOut
defaultOut CPUState{..} = CPUOut
  { _memAddr = _pc
  , _memWrite = Nothing
  , _vidAddr = 0
  , _vidWrite = Nothing
  }

type CPU = CPUM CPUState CPUOut

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine = runCPU defaultOut . step

step :: Pure CPUIn -> CPU ()
step CPUIn{..} = do
    randomState %= lfsr
    when tick $ timer %= fromMaybe 0 . predIdx

    use phase >>= \case
        Init -> phase .= Fetch
        Fetch -> do
            pc += 1
            phase .= Exec memRead
        ClearFB y -> clearScreen y
        WriteBCD x i -> case succIdx i of
            Nothing -> phase .= Fetch
            Just i' -> do
                addr <- uses ptr (+ fromIntegral i')
                writeMem addr (toBCDRom x !! i')
                phase .= WriteBCD x i'
        WaitKeyRelease reg prevState -> do
            case keyRelease prevState keyState of
                Just key -> do
                    setReg reg $ fromIntegral key
                    phase .= Fetch
                Nothing -> do
                    phase .= WaitKeyRelease reg keyState
        StoreReg reg -> case predIdx reg of
            Nothing -> phase .= Fetch
            Just reg' -> storeReg reg'
        LoadReg reg -> do
            setReg reg memRead
            case predIdx reg of
                Nothing -> phase .= Fetch
                Just reg' -> do
                    addr <- uses ptr (+ fromIntegral reg')
                    memAddr .:= addr
                    phase .= LoadReg reg'
        DrawRead x y row -> do
           addr <- uses ptr (+ fromIntegral row)
           vidAddr .:= y + fromIntegral row
           memAddr .:= addr
           phase .= DrawWrite x y row
        DrawWrite x y row -> do
           let bg = vidRead
               sprite = bitCoerce memRead ++ repeat low
               sprite' = bitCoerce sprite `shiftR` fromIntegral x
               pattern = bg `xor` sprite'
               collision = (bg .&. sprite') /= 0
           when collision $ setFlag high
           writeVid (y + fromIntegral row) pattern
           phase .= maybe Fetch (DrawRead x y) (predIdx row)
        Exec hi -> do
            let lo = memRead
            pc += 1
            phase .= Fetch
            case traceShowId $ decodeInstr hi lo of
                ClearScreen -> do
                    clearScreen 0
                Ret -> do
                    popPC
                Jump addr -> do
                    pc .= addr
                Call addr -> do
                    pushPC
                    pc .= addr
                -- Sys n -> do
                --     errorX $ unwords ["Unimplemented: SYS", show n]
                SkipEqImmIs b regX imm -> do
                    x <- getReg regX
                    when ((x == imm) == b) skip
                SkipEqRegIs b regX regY -> do
                    x <- getReg regX
                    y <- getReg regY
                    when ((x == y) == b) skip
                PutImm regX imm -> do
                    setReg regX imm
                AddImm regX imm -> do
                    x <- getReg regX
                    setReg regX (x + imm)
                ALU fun regX regY -> do
                    x <- getReg regX
                    y <- getReg regY
                    let (flag, x') = alu fun x y
                    setReg regX x'
                    maybe (return ()) setFlag flag
                SetPtr addr -> do
                    ptr .= addr
                JumpPlusR0 addr -> do
                    offset <- getReg 0
                    pc .= addr + fromIntegral offset
                Randomize regX mask -> do
                    rnd <- fromIntegral <$> use randomState
                    setReg regX $ rnd .&. mask
                DrawSprite regX regY height -> do
                    x <- fromIntegral <$> getReg regX
                    y <- fromIntegral <$> getReg regY
                    let height' = if height == 0 then 15 else height - 1
                    setFlag low
                    -- We draw from bottom to top. This allows using
                    -- the remaining height being 0 as a stopping
                    -- condition.
                    phase .= DrawRead x y height'
                SkipKeyIs b regX -> do
                    key <- fromIntegral <$> getReg regX
                    let isPressed = keyState !! key
                    when (isPressed == b) skip
                WaitKey regX -> phase .= WaitKeyRelease regX keyState
                GetTimer regX -> do
                    setReg regX =<< use timer
                SetTimer regX -> do
                    val <- getReg regX
                    timer .= val
                SetSound regX -> do
                    return () -- TODO
                AddPtr regX -> do
                    x <- getReg regX
                    ptr += fromIntegral x
                LoadFont regX -> do
                    x <- getReg regX
                    ptr .= toFont x
                StoreBCD regX -> do
                    x <- getReg regX
                    addr <- use ptr
                    writeMem addr $ toBCDRom x !! 0
                    phase .= WriteBCD x 0
                StoreRegs regMax -> storeReg regMax
                LoadRegs regMax -> do
                    addr <- uses ptr (+ fromIntegral regMax)
                    memAddr .:= addr
                    phase .= LoadReg regMax
                op -> fatal "Exec" op
  where
    clearScreen y = do
        writeVid y 0
        phase .= maybe Fetch ClearFB (succIdx y)

    storeReg reg = do
        addr <- uses ptr (+ fromIntegral reg)
        writeMem addr =<< getReg reg
        phase .= StoreReg reg

    setReg reg val = registers %= replace reg val
    getReg reg = uses registers (!! reg)
    setFlag = setReg 0xf . fromIntegral

    writeMem addr val = do
        memAddr .:= addr
        memWrite .:= Just val

    writeVid y val = do
        vidAddr .:= y
        vidWrite .:= Just val

    popPC = do
       (pc', stack') <- pop <$> use stack
       stack .= stack'
       pc .= pc'

    pushPC = do
       pc <- use pc
       stack %= push pc

    skip = pc += 2

toBCDRom :: Byte -> Vec 3 Byte
toBCDRom = asyncRom $(listToVecTH $ fmap toBCD [minBound..maxBound])

keyRelease :: KeypadState -> KeypadState -> Maybe Key
keyRelease prev new = elemIndex True $ zipWith (\ before now -> before && not now) prev new
