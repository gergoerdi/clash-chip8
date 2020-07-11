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
import Data.Foldable (for_, traverse_)
import Data.Maybe
import Data.Word

import Debug.Trace
import Text.Printf

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
    | ClearFB VidY
    | DrawRead VidX VidY Nybble
    | DrawWrite VidX VidY Nybble
    | WaitKeyRelease Reg KeypadState
    | WriteRegs Reg
    | ReadRegs Reg
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
makeLenses ''CPUState

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

cpu :: (HiddenClockResetEnable dom) => Signals dom CPUIn -> Signals dom CPUOut
cpu = mealyCPU initState defaultOut step

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
        WaitKeyRelease reg prevState -> do
            case keyRelease prevState keyState of
                Just key -> do
                    setReg reg $ fromIntegral key
                    phase .= Fetch
                Nothing -> do
                    phase .= WaitKeyRelease reg keyState
        WriteBCD x i -> do
            addr <- uses ptr (+ fromIntegral i)
            writeMem addr . fromIntegral $ toBCD x !! i
            phase .= maybe Init (WriteBCD x) (succIdx i)
        WriteRegs reg -> do
            addr <- uses ptr (+ fromIntegral reg)
            writeMem addr =<< getReg reg
            phase .= maybe Init WriteRegs (predIdx reg)
        ReadRegs reg -> do
            setReg reg memRead
            case predIdx reg of
                Nothing -> phase .= Fetch
                Just reg' -> do
                    addr <- uses ptr (+ fromIntegral reg')
                    memAddr .:= addr
                    phase .= ReadRegs reg'
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
                Sys n -> do
                    errorX $ printf "Unimplemented: SYS %04x" (fromIntegral n :: Word16)
                SkipEqImmIs b vx imm -> do
                    x <- getReg vx
                    when ((x == imm) == b) skip
                SkipEqRegIs b vx vy -> do
                    x <- getReg vx
                    y <- getReg vy
                    when ((x == y) == b) skip
                LoadImm vx imm -> do
                    setReg vx imm
                AddImm vx imm -> do
                    x <- getReg vx
                    setReg vx (x + imm)
                Arith fun vx vy -> do
                    x <- getReg vx
                    y <- getReg vy
                    let (flag, x') = alu fun x y
                    setReg vx x'
                    traverse_ setFlag flag
                LoadPtr addr -> do
                    ptr .= addr
                JumpPlusV0 addr -> do
                    offset <- getReg 0
                    pc .= addr + fromIntegral offset
                Randomize vx mask -> do
                    rnd <- fromIntegral <$> use randomState
                    setReg vx $ rnd .&. mask
                DrawSprite vx vy height -> do
                    x <- fromIntegral <$> getReg vx
                    y <- fromIntegral <$> getReg vy
                    let height' = if height == 0 then 15 else height - 1
                        height'' = truncateB $ satAdd SatBound y (extend height') - y
                    setFlag low
                    -- We draw from bottom to top. This allows using
                    -- the remaining height being 0 as a stopping
                    -- condition.
                    phase .= DrawRead x y height''
                SkipKeyIs b vx -> do
                    key <- toKey <$> getReg vx
                    let isPressed = keyState !! key
                    when (isPressed == b) skip
                WaitKey vx -> phase .= WaitKeyRelease vx keyState
                GetTimer vx -> do
                    setReg vx =<< use timer
                LoadTimer vx -> do
                    val <- getReg vx
                    timer .= val
                LoadSound vx -> do
                    return () -- TODO
                AddPtr vx -> do
                    x <- getReg vx
                    ptr += fromIntegral x
                LoadHex vx -> do
                    x <- getReg vx
                    ptr .= toHex x
                StoreBCD vx -> do
                    x <- getReg vx
                    phase .= WriteBCD x 0
                StoreRegs regMax -> do
                    phase .= WriteRegs regMax
                LoadRegs regMax -> do
                    addr <- uses ptr (+ fromIntegral regMax)
                    memAddr .:= addr
                    phase .= ReadRegs regMax
  where
    clearScreen y = do
        writeVid y 0
        phase .= maybe Fetch ClearFB (succIdx y)

    setReg reg val = registers %= replace reg val
    getReg reg = uses registers (!! reg)
    setFlag = setReg 0xf . fromIntegral

    toKey :: Byte -> Key
    toKey x = bitCoerce key
      where
        (_, key) = bitCoerce x :: (Nybble, Nybble)

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

keyRelease :: KeypadState -> KeypadState -> Maybe Key
keyRelease prev new = fmap bitCoerce $ elemIndex True $ zipWith (\ before now -> before && not now) prev new
