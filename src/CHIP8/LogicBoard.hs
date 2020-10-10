{-# LANGUAGE RecordWildCards #-}
module CHIP8.LogicBoard (logicBoard) where

import Clash.Prelude hiding (rom)
import qualified Clash.Prelude as C
import RetroClash.Utils
import RetroClash.Memory
import Data.Maybe

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font

import Data.Functor.Barbie
import Barbies.Bare

logicBoard
    :: (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom Bool
    -> Signal dom KeypadState
    -> Signal dom (Maybe VidRow)
    -> ( Signal dom VidY
       , Signal dom (Maybe VidRow)
       )
logicBoard programFile tick keyState vidRead = (_vidAddr, _vidWrite)
  where
    CPUOut{..} = cpu CPUIn{..}

    memRead = fmap (fromMaybe 0) $ memoryMap_ (Just <$> _memAddr) _memWrite $ do
        -- Use TH to force `hexDigits` into normal form, otherwise Clash synthesis fails
        mask @9 0x000 $ rom $ C.rom $(lift hexDigits)
        offset 0x200 $ ram $ packRam $ blockRamFile (SNat @(0x1000 - 0x200)) programFile
