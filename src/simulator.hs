{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
module Main where

import Prelude

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font

import RetroClash.Sim.SDL
import RetroClash.Barbies
import Control.Monad.State
import Data.Word
import Data.Array ((!))
import Data.Array.IO
import Data.ByteString as BS
import Data.Foldable
import Data.Bits
import Debug.Trace

world :: IOUArray Word16 Word8 -> IOUArray Word8 Word64 -> Bool -> Pure CPUOut -> IO (Pure CPUIn)
world ram vid firstForFrame CPUOut{..} = do
    memRead <- readMem _memAddr
    vidRead <- readVid _vidAddr
    tick <- return firstForFrame

    traverse_ print _vidWrite

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
        ram <- newArray (0, 4095) 0
        zipWithM_ (writeArray ram) [0x000..] (toList hexDigits)

        img <- BS.readFile "roms/2048.ch8"
        zipWithM_ (writeArray ram) [0x200..] (BS.unpack img)
        return ram
    vid <- newArray (0, 31) 0

    let sim firstForFrame = do
            (inp, s) <- get
            let (out, s') = runState (cpuMachine inp) s
            inp' <- liftIO $ world ram vid firstForFrame out
            put (inp', s')


    let initInput = CPUIn
            { memRead = 0
            , vidRead = 0
            , tick = False
            }
    flip evalStateT (initInput, initState) $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        sim True
        replicateM_ 10_000 $ sim False

        vidArr <- liftIO $ freeze vid
        return $ rasterizePattern @64 @32 $ \x y ->
          let fg = (0xe7, 0xc2, 0x51)
              bg = (0x20, 0x20, 0x20)
              row = vidArr ! fromIntegral y
          in if testBit row (fromIntegral (maxBound - x)) then fg else bg
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 20
        , screenRefreshRate = 60
        , reportFPS = True
        }
