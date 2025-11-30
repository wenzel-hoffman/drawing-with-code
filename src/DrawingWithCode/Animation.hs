{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animation
  ( makeAnimationRenderProgram
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified Data.ByteString.Builder as BSB
import qualified Control.Monad as CM
import DrawingWithCode.ArgsParse (parseArgs)
import qualified DrawingWithCode.Render as Render

makeAnimationRenderProgram ∷ String → (FS.FrameState → BSB.Builder) → IO ()
makeAnimationRenderProgram exeName mkFrame =
  parseArgs exeName >>= \(outDir, (w, h), fps, seconds, threads) -> do
    locks ← Render.renderAnimation outDir mkFrame (w, h) fps seconds threads
    CM.forM_ locks (\x → x.waitForLock)
    putStrLn $ "[OK] All " <> show threads.unThreads <> " thread(s) are done"
