{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
module Main where

import Clash.Prelude

import CHIP8.Types
import CHIP8.LogicBoard
import CHIP8.Sim

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import Control.Monad.State
import Data.Word
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Foldable (for_)

import System.IO
import System.IO.Temp
import qualified Data.List as L

world
    :: IOUArray Word8 Word64
    -> Maybe (VidY, VidRow)
    -> IO ()
world vid vidWrite = for_ vidWrite $ \(addr, row) -> do
    writeArray vid (fromIntegral addr) row

main :: IO ()
main = withSystemTempFile "chip8-.bin" $ \romFile romHandle -> do
    img <- BS.readFile "roms/hidden.ch8"
    hPutStr romHandle $ unlines $ binLines (Just (0x1000 - 0x0200)) (BS.unpack img)
    hClose romHandle

    vid <- newArray (0, 31) 0

    sim <- simulateIO_ @System
           (uncurry (logicBoard romFile) . unbundle)
           (False, repeat False)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown keyboardMap

        let step firstForFrame = do
                sim $ \vidWrite -> do
                    liftIO $ world vid vidWrite
                    return (firstForFrame, keyState)
        step True
        replicateM_ 1000 $ step False

        rasterizeVideoBuf vid
  where
    videoParams = MkVideoParams
        { windowTitle = "CHIP-8"
        , screenScale = 20
        , screenRefreshRate = 60
        , reportFPS = True
        }

binLines :: Maybe Int -> [Word8] -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
