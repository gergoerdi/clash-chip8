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
import Options.Applicative

world
    :: IOUArray Addr Word8
    -> IOUArray VidY Word64
    -> KeypadState
    -> Bool
    -> Pure CPUOut
    -> IO (Pure CPUIn)
world ram vid keyState tick CPUOut{..} = do
    memRead <- readArray ram _memAddr
    vidRead <- Just <$> readArray vid _vidAddr

    traverse_ (writeArray ram _memAddr) _memWrite
    traverse_ (writeArray vid _vidAddr) _vidWrite

    return CPUIn{..}

main :: IO ()
main = do
    filePath <- execParser optionsInfo

    ram <- do
        ram <- newArray (minBound, maxBound) 0
        zipWithM_ (writeArray ram) [0x000..] (toList hexDigits)

        img <- BS.readFile filePath
        zipWithM_ (writeArray ram) [0x200..] (BS.unpack img)
        return ram
    vid <- newArray (minBound, maxBound) 0

    let initInput = CPUIn
            { memRead = 0
            , vidRead = Nothing
            , tick = False
            , keyState = repeat False
            }
    flip evalStateT (initInput, initState) $
      withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown keyboardMap
            sim tick = do
                (inp, s) <- get
                let (out, s') = runState (cpuMachine inp) s
                inp' <- liftIO $ world ram vid keyState tick out
                put (inp', s')

        sim True
        replicateM_ 5000 $ sim False

        rasterizeVideoBuf vid
