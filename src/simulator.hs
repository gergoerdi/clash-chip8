{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Clash.Prelude hiding ((!))

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font
import CHIP8.Input

import RetroClash.Sim.SDL
import RetroClash.Barbies
import RetroClash.Keypad
import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Bits
import Debug.Trace
import SDL.Event as SDL
import SDL.Input.Keyboard
import SDL.Input.Keyboard.Codes

world
    :: IOUArray Word16 Word8
    -> IOUArray Word8 Word64
    -> KeypadState
    -> Bool
    -> Pure CPUOut
    -> IO (Pure CPUIn)
world ram vid keyState tick CPUOut{..} = do
    memRead <- readMem _memAddr
    vidRead <- readVid _vidAddr

    traverse_ (writeMem _memAddr) _memWrite
    traverse_ (writeVid _vidAddr) _vidWrite

    return CPUIn{..}
  where
    readMem addr = readArray ram (fromIntegral addr)
    readVid addr = readArray vid (fromIntegral addr)

    writeMem addr = writeArray ram (fromIntegral addr)
    writeVid addr = writeArray vid (fromIntegral addr)

keyboardLayout :: Matrix 4 4 Scancode
keyboardLayout =
    (Scancode1 :> Scancode2 :> Scancode3 :> Scancode4 :> Nil) :>
    (ScancodeQ :> ScancodeW :> ScancodeE :> ScancodeR :> Nil) :>
    (ScancodeA :> ScancodeS :> ScancodeD :> ScancodeF :> Nil) :>
    (ScancodeZ :> ScancodeX :> ScancodeC :> ScancodeV :> Nil) :>
    Nil

keyboardMap :: Vec 16 Scancode
keyboardMap = scatter (repeat ScancodeUnknown) (concat layout) (concat keyboardLayout)

main :: IO ()
main = do
    ram <- do
        ram <- newArray (0x000, 0xfff) 0
        zipWithM_ (writeArray ram) [0x000..] (toList hexDigits)

        img <- BS.readFile "roms/hidden.ch8"
        zipWithM_ (writeArray ram) [0x200..] (BS.unpack img)
        return ram
    vid <- newArray (0, 31) 0

    let initInput = CPUIn
            { memRead = 0
            , vidRead = 0
            , tick = False
            , keyState = repeat False
            }
    flip evalStateT (initInput, initState) $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown keyboardMap
            sim firstForFrame = do
                (inp, s) <- get
                let (out, s') = runState (cpuMachine inp) s
                inp' <- liftIO $ world ram vid keyState firstForFrame out
                put (inp', s')

        sim True
        replicateM_ 5000 $ sim False

        vidArr <- liftIO $ freeze vid
        return $ rasterizePattern @64 @32 $ \x y ->
          let fg = (0xe7, 0xc2, 0x51)
              bg = (0x50, 0x50, 0x50)
              row = vidArr ! fromIntegral y
          in if testBit row (fromIntegral (maxBound - x)) then fg else bg
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 20
        , screenRefreshRate = 60
        , reportFPS = True
        }
