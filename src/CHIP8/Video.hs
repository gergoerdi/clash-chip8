{-# LANGUAGE RecordWildCards, NumericUnderscores, ApplicativeDo #-}
{-# LANGUAGE MonomorphismRestriction #-}
module CHIP8.Video where

import Clash.Prelude
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Utils
import RetroClash.Delayed

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
video write = (frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = fromSignal $ scale @64 (SNat @10) vgaX
    vgaY' = fromSignal $ scale @32 (SNat @10) . center $ vgaY

    rgb = maybe border palette <$> pixel

    border = (0x30, 0x30, 0x50)

    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xcc, 0x33)

    pixel = enable (delayI False visible) $ msb <$> row
    visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

    address = bitCoerce <$> mux newY vgaY' (pure Nothing)
    load = delayedRam (blockRam1 ClearOnReset (SNat @32) 0) (fromMaybe 0 <$> address) (fromSignal write)

    lineStart = liftD (isRising False) $ (isJust <$> vgaX')
    newY = liftD (changed Nothing) vgaY'
    newX = visible .&&. liftD (changed Nothing) vgaX'

    row = delayedRegister 0 $ \row ->
        mux (delayI False $ isJust <$> address) load $
        mux (delayI False newX) ((`rotateL` 1) <$> row) $
        row
