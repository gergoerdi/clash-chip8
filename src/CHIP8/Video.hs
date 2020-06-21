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

    vgaX' = fromSignal $ scale (SNat @9) . center @(9 * 64) $ vgaX
    vgaY' = fromSignal $ scale (SNat @9) . center @(9 * 32) $ vgaY

    rgb = maybe border palette <$> pixel

    lineStart = isRisingD False $ (isJust <$> vgaX')
    newX = changedD Nothing vgaX'
    visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

    addr = mux lineStart vgaY' (pure Nothing)
    write' = fmap (first bitCoerce) <$> fromSignal write
    load = delayedBlockRam1 ClearOnReset (SNat @32) 0x00 (fromMaybe 0 <$> addr) write'

    row = delayedRegister 0 $ \row ->
        mux (delayI False $ isJust <$> addr) load $
        mux (delayI False newX) ((`shiftL` 1) <$> row) $
        row
    pixel = enable (delayI False visible) $ msb <$> row

    border = (0x30, 0x30, 0x50)

    palette :: Bit -> (Unsigned 8, Unsigned 8, Unsigned 8)
    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xff, 0xff)
