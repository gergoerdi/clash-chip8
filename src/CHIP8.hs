{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module CHIP8 where

import CHIP8.LogicBoard
import CHIP8.Video
import CHIP8.Input

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Keypad
import RetroClash.Video
import RetroClash.Clock

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "ROWS"      ::: Signal Dom25 (Vec 4 (Active Low))
    -> ( "COLS"    ::: Signal Dom25 (Vec 4 (Active Low))
       , "VGA"     ::: VGAOut Dom25 8 8 8
       )
topEntity = withEnableGen board
  where
    board rows = (cols, vga)
      where
        (cols, keypadState) = scanKeypad rows
        keyState =
            debounce (SNat @(Milliseconds 5)) (repeat False) $
            scatter (repeat False) (concat layout) <$> (concat <$> keypadState)

        (frameEnd, vidRead, vga) = video vidAddr vidWrite
        (vidAddr, vidWrite) = logicBoard "image.bin" frameEnd keyState vidRead

makeTopEntity 'topEntity
