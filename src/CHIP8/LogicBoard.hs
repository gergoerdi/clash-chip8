{-# LANGUAGE RecordWildCards #-}
module CHIP8.LogicBoard (logicBoard) where

import Clash.Prelude
import RetroClash.Utils
import Control.Arrow (second)

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font

import Data.Functor.Barbie
import Barbies.Bare

import Debug.Trace

data Mem dom a d
    = RAM (Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d)
    | ROM (Signal dom a -> Signal dom d)

data MemSpec dom a d
    = UpTo a (Mem dom a d) (MemSpec dom a d)
    | Default (Mem dom a d)

memoryMap
    :: (Ord a, Num a, HiddenClockResetEnable dom)
    => MemSpec dom a d
    -> Signal dom a
    -> Signal dom (Maybe d)
    -> Signal dom d
memoryMap mems addr wr = go mems addr
  where
    go mems addr = case mems of
        UpTo lim mem mems -> mux (register False $ addr .< lim) (connect mem) (go mems $ subtract lim <$> addr)
        Default mem -> connect mem
      where
        connect (RAM ram) = ram addr (packWrite <$> addr <*> wr)
        connect (ROM rom) = rom addr

logicBoard
    :: forall dom. (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom Bool
    -> Signal dom KeypadState
    -> Signal dom (Maybe (VidY, VidRow))
logicBoard programFile tick keyState = vidOut
  where
    CPUOut{..} = cpu CPUIn{..}
    vidOut = packWrite <$> _vidAddr <*> _vidWrite

    memRead = memoryMap mems _memAddr _memWrite
      where
        mems = UpTo 0x0200 (ROM fontROM) $
               Default (RAM ram)

    fontROM = romPow2 hexDigits . fmap fromIntegral
    ram addr wr = unpack <$> blockRamFile (SNat @(0x1000 - 0x200)) programFile addr wr'
      where
        wr' = fmap (second pack) <$> wr

    vidRead = blockRam1 ClearOnReset (SNat @32) 0 _vidAddr vidOut
