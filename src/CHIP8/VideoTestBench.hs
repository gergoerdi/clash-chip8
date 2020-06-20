{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module CHIP8.VideoTestBench where

import CHIP8.Types
import CHIP8.LogicBoard
import CHIP8.Video
import CHIP8.Input

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "KEYS"      ::: Signal Dom25 KeypadState
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board keyState = vga
      where
        (frameEnd, vga) = video vidWrite
        vidWrite = logicBoard "image.bin" frameEnd keyState

        -- ptr = regEn 0 frameEnd $ ptr + 1
        -- val = regEn 0xaa_aa_aa_aa_aa_aa_aa_aa frameEnd $ complement <$> val
        -- vidWrite = packWrite <$> ptr <*> (Just <$> val)

makeTopEntity 'topEntity
