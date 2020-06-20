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

    vgaX' = fromSignal $ scale (SNat @10) vgaX
    vgaY' = fromSignal $ scale (SNat @10) . center @320 $ vgaY

    rgb = maybe border monochrome <$> pixel

    row = delayedBlockRam1 ClearOnReset (SNat @32) 0x00 (fromMaybe 0 <$> vgaY') (fmap (first bitCoerce) <$> fromSignal write)
    pixel = do
        row <- enable (delayI False $ isJust <$> vgaY') row
        x <- delayI Nothing vgaX'
        pure $ (!) <$> row <*> x

    border = (0x30, 0x30, 0x50)
