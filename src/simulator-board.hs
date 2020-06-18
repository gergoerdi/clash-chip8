{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
module Main where

import Clash.Prelude hiding ((!))

import CHIP8.Types
import CHIP8.LogicBoard
import CHIP8.Input

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
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

import System.IO
import System.IO.Temp
import qualified Data.List as L

world
    :: IOUArray Word8 Word64
    -> Maybe (VidY, VidRow)
    -> IO ()
world vid vidWrite = for_ vidWrite $ \(addr, row) -> do
    writeArray vid (fromIntegral addr) row

scanLayout :: Matrix 4 4 Scancode
scanLayout =
    (Scancode1 :> Scancode2 :> Scancode3 :> Scancode4 :> Nil) :>
    (ScancodeQ :> ScancodeW :> ScancodeE :> ScancodeR :> Nil) :>
    (ScancodeA :> ScancodeS :> ScancodeD :> ScancodeF :> Nil) :>
    (ScancodeZ :> ScancodeX :> ScancodeC :> ScancodeV :> Nil) :>
    Nil

scanMap :: Vec 16 Scancode
scanMap = scatter (repeat ScancodeUnknown) (concat layout) (concat scanLayout)

main :: IO ()
main = withSystemTempFile "chip8-.rom" $ \romFile romHandle -> do
    img <- BS.readFile "roms/hidden.ch8"
    hPutStr romHandle $ unlines $ binLines (Just (0x1000 - 0x0200)) (BS.unpack img)
    hClose romHandle

    vid <- newArray (0, 31) 0

    sim <- simulateIO_ @System
           (uncurry (logicBoard romFile) . unbundle)
           (False, repeat False)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let keyState = fmap keyDown scanMap

        let step firstForFrame = do
                sim $ \vidWrite -> do
                    liftIO $ world vid vidWrite
                    return (firstForFrame, keyState)
        step True
        replicateM_ 1_000 $ step False

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

binLines :: Maybe Int -> [Word8] -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
