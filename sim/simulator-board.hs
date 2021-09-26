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
import Data.Tuple.Extra (uncurry3)
import qualified Data.ByteString as BS
import Data.Foldable (for_, traverse_)

import System.IO
import System.IO.Temp
import qualified Data.List as L
import Options.Applicative

world
    :: IOUArray VidY Word64
    -> VidY
    -> Maybe VidRow
    -> IO VidRow
world vid vidAddr vidWrite = do
    traverse_ (writeArray vid vidAddr) vidWrite
    readArray vid vidAddr

main :: IO ()
main = withSystemTempFile "chip8-.bin" $ \romFile romHandle -> do
    filePath <- execParser optionsInfo

    img <- BS.readFile filePath
    hPutStr romHandle $ unlines $ binLines (Just (0x1000 - 0x0200)) (BS.unpack img)
    hClose romHandle

    vid <- newArray (minBound, maxBound) 0

    sim <- simulateIO_ @System
           (bundle . uncurry3 (logicBoard romFile) . unbundle)
           (False, repeat False, Nothing)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown keyboardMap

        let step i = sim $ \(vidAddr, vidWrite) -> do
                let tick = i == (0 :: Int)
                    allowVideoAccess = i `mod` 23 == 1
                vidRead <- liftIO $ world vid vidAddr vidWrite
                return (tick, keyState, vidRead <$ guard allowVideoAccess)
        mapM_ step [0..1000]

        rasterizeVideoBuf vid

binLines :: Maybe Int -> [Word8] -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
