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

    memRead = 0 |>. $(memoryMap_ @Addr [| {- Just <$> -} _memAddr|] [|_memWrite|] $ do
        -- Use TH to force `hexDigits` into normal form, otherwise Clash synthesis fails
        font <- romFromVec (SNat @0x0200) (lift (hexDigits ++ repeat 0 :: Vec 0x200 Byte))
        ram <- ramFromFile (SNat @(0x1000 - 0x200)) [|programFile|]

        from 0x000 $ connect font
        from 0x200 $ connect ram)
