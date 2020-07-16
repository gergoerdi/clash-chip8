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
    -> Signal dom (Maybe VidRow)
    -> ( Signal dom VidY
       , Signal dom (Maybe VidRow)
       )
logicBoard programFile tick keyState vidRead = (_vidAddr, _vidWrite)
  where
    CPUOut{..} = cpu CPUIn{..}

    memRead = memory memSpec _memAddr _memWrite

    -- Use TH to force `hexDigits` into normal form, otherwise Clash synthesis fails
    memSpec =
        UpTo    0x200 (ROM $ rom $(lift hexDigits)) $
        Default       (RAM $ packRAM $ blockRamFile (SNat @(0x1000 - 0x200)) programFile)
