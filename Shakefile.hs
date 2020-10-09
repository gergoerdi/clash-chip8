{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude hiding (lift)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Trans.Class

outDir :: FilePath
outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    outDir </> "image.bin" %> \out -> do
        imageFile <- fromMaybe "roms/hidden.ch8" <$> getConfig "IMAGE"
        binImage (Just $ 0x1000 - 0x0200) imageFile out

    kit@ClashKit{..} <- clashRules outDir "clash" Verilog
        [ "src" ]
        "CHIP8"
        [ "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=600"
        ] $
        need [outDir </> "image.bin"]
    phony "clashi" $ clash ["--interactive", unBuildDir "src/CHIP8.hs"]

    let targets =
            [ ("nexys-a7-50t", xilinxVivado nexysA750T)
            -- , ("papilio-pro", xilinxISE papilioPro)
            -- , ("papilio-one", xilinxISE papilioOne)
            ]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name) ("target" </> name) "Top"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):phonies
