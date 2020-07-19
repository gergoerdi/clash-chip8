{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font
import CHIP8.Sim

import RetroClash.Sim.SDL
import RetroClash.Barbies
import Control.Monad.State
import Data.Word
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)

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

        rasterizeVideoBuf vid
