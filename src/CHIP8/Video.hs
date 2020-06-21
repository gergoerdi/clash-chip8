{-# LANGUAGE RecordWildCards, NumericUnderscores, ApplicativeDo #-}
module CHIP8.Video where

import Clash.Prelude
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Utils

import Data.Maybe
import Control.Arrow (first)
import CHIP8.Types
import Control.Monad (join)

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 (Maybe (VidY, VidRow))
    -> (Signal Dom25 Bool, VGAOut Dom25 8 8 8)
video write = (matchDelay rgb False frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = scale (SNat @9) . center @(9 * 64) $ vgaX
    vgaY' = scale (SNat @9) . center @(9 * 32) $ vgaY

    rgb = maybe border palette <$> pixel

    lineStart = isRising False (isJust <$> vgaX')
    newX = vgaX' ./=. register Nothing vgaX'

    addr = join <$> enable lineStart vgaY'
    load = blockRam1 ClearOnReset (SNat @32) 0x00 (fromMaybe 0 <$> addr) (fmap (first bitCoerce) <$> write)

    row = register 0 $
          mux (register False $ isJust <$> addr) load $
          mux (register False newX) ((`shiftL` 1) <$> row) $
          row
    pixel = unsafeFromSignal @_ @_ @2 $ enable (register False . register False $ isJust <$> vgaX' .&&. isJust <$> vgaY') $
            msb <$> row

    border = (0x30, 0x30, 0x50)

    palette :: Bit -> (Unsigned 8, Unsigned 8, Unsigned 8)
    palette 0 = minBound
    palette 1 = maxBound
