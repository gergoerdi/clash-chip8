{-# LANGUAGE RecordWildCards #-}
module CHIP8.LogicBoard (logicBoard) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Memory
import Data.Maybe

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font

import Data.Functor.Barbie
import Barbies.Bare
import qualified Language.Haskell.TH.Syntax as TH

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

    fontAddr = enable (_memAddr .< 0x200) _memAddr
    ramAddr = enable (0x200 <=. _memAddr) (_memAddr - 0x200)

    -- Use TH to force `hexDigits` into normal form, otherwise Clash synthesis fails
    font = rom $(TH.lift hexDigits) (fromJustX <$> fontAddr)
    ram = packRam (blockRamFile (SNat @(0x1000 - 0x200)) programFile)
            (fromJustX <$> ramAddr)
            (liftA2 (,) <$> ramAddr <*> _memWrite)

    memRead = muxA
        [ enable (register False $ isJust <$> fontAddr) font
        , enable (register False $ isJust <$> ramAddr) ram
        ] .<|
        0
