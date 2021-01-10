{-# LANGUAGE RecordWildCards, NumericUnderscores, ApplicativeDo #-}
{-# LANGUAGE MonomorphismRestriction, ViewPatterns #-}
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
    => Signal Dom25 VidY
    -> Signal Dom25 (Maybe VidRow)
    -> ( Signal Dom25 Bool
       , Signal Dom25 (Maybe VidRow)
       , VGAOut Dom25 8 8 8
       )
video (fromSignal -> cpuAddr) (fromSignal -> write) = (frameEnd, toSignal cpuRead, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    vgaX' = fromSignal $ fst . scale @64 (SNat @10) $ vgaX
    vgaY' = fromSignal $ fst . scale @32 (SNat @10) . center $ vgaY

    rgb = maybe border palette <$> pixel

    border = (0x30, 0x30, 0x50)

    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0xff, 0xcc, 0x33)

    pixel = enable (delayI False visible) $ msb <$> row
    visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

    vgaAddr = bitCoerce <$> guardA newY vgaY'

    -- vblank = fromSignal $ isNothing <$> vgaY
    -- addr = mux vblank cpuAddr (fromMaybe 0 <$> vgaAddr)
    addr = fromMaybe <$> cpuAddr <*> vgaAddr
    load = delayedRam (blockRam1 ClearOnReset (SNat @32) 0) addr
        (packWrite <$> cpuAddr <*> write)

    -- cpuRead = enable (delayI False vblank) load
    cpuRead = enable (delayI False $ isNothing <$> vgaAddr) load

    lineStart = liftD (isRising False) $ (isJust <$> vgaX')
    newY = liftD (changed Nothing) vgaY'
    newX = visible .&&. liftD (changed Nothing) vgaX'

    row = delayedRegister 0 $ \row ->
        mux (delayI False $ isJust <$> vgaAddr) load $
        mux (delayI False newX) ((`rotateL` 1) <$> row) $
        row
