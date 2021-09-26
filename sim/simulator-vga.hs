{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
module Main where

import Clash.Prelude hiding ((!))

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import CHIP8.Input

import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
import RetroClash.Barbies
import RetroClash.Keypad

import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Bits
import Debug.Trace
import SDL.Event as SDL
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes
import Control.Monad.Extra

import System.IO
import System.IO.Temp
import qualified Data.List as L

scanLayout :: Matrix 4 4 Scancode
scanLayout =
    (Scancode1 :> Scancode2 :> Scancode3 :> Scancode4 :> Nil) :>
    (ScancodeQ :> ScancodeW :> ScancodeE :> ScancodeR :> Nil) :>
    (ScancodeA :> ScancodeS :> ScancodeD :> ScancodeF :> Nil) :>
    (ScancodeZ :> ScancodeX :> ScancodeC :> ScancodeV :> Nil) :>
    Nil

scanMap :: Vec 16 Scancode
scanMap = scatter (repeat ScancodeUnknown) (concat layout) (concat scanLayout)

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    buf <- newBufferArray

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown scanMap

        let input = INPUT
                { iRESET = low
                , iKEYS = bitCoerce keyState
                }

        whileM $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (oVGA_HSYNC, oVGA_VSYNC, (oVGA_RED, oVGA_GREEN, oVGA_BLUE))
            fmap not $ vgaSinkBuf vga640x480at60 buf vgaOut

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }

binLines :: Maybe Int -> [Word8] -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
