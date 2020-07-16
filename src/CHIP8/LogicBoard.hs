{-# LANGUAGE RecordWildCards #-}
module CHIP8.LogicBoard (logicBoard) where

import Clash.Prelude
import RetroClash.Utils
import Control.Arrow (second)

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font
import CHIP8.Memory

import Data.Functor.Barbie
import Barbies.Bare

logicBoard
    :: (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom Bool
    -> Signal dom KeypadState
    -> Signal dom (Maybe (VidY, VidRow))
logicBoard programFile tick keyState = vidOut
  where
    CPUOut{..} = cpu CPUIn{..}
    vidOut = packWrite <$> _vidAddr <*> _vidWrite

    memRead = memory memSpec _memAddr _memWrite
    vidRead = blockRam1 ClearOnReset (SNat @32) 0 _vidAddr vidOut

    -- Use TH to force `hexDigits` into normal form, otherwise Clash synthesis fails
    memSpec =
        UpTo    0x200 (ROM $ rom $(lift hexDigits)) $
        Default       (RAM $ packRAM $ blockRamFile (SNat @(0x1000 - 0x200)) programFile)
