{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module CHIP8 where

import CHIP8.CPU
import CHIP8.LogicBoard

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Keypad
import RetroClash.Video
import RetroClash.Clock

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "ROWS"      ::: Signal System (Vec 4 (Active Low))
    -> ( "COLS"    ::: Signal System (Vec 4 (Active Low))
       , "VGA"     ::: VGAOut Dom25 8 8 8
       )
topEntity = withEnableGen board
  where
    board rows = (cols, vga)
      where
        cols = pure $ repeat $ toActive False

        VGADriver{..} = vgaDriver vga640x480at60
        vga = vgaOut vgaSync $ pure (0, 0, 0)

makeTopEntity 'topEntity
