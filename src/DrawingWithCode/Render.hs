{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DrawingWithCode.Render
  ( renderAnimation
  , renderFramesRangeThread
  , mkFrameFileName
  , renderFrame
  , Fps (..)
  , Seconds (..)
  , Threads (..)
  , Lock (..)
  ) where

import qualified System.IO as SysIO
import qualified DrawingWithCode.FrameState as FS
import qualified Data.ByteString.Builder as BSB
import qualified Control.Monad as CM
import DrawingWithCode.Ranges (splitRange)
import Text.Printf (printf)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent (forkIO)

newtype Fps = Fps { unFps ∷ Word } deriving (Eq, Show)
newtype Seconds = Seconds { unSeconds ∷ Word } deriving (Eq, Show)
newtype Threads = Threads { unThreads ∷ Word } deriving (Eq, Show)
newtype Lock = Lock { waitForLock ∷ IO () }

renderAnimation
  ∷ FilePath
  → (FS.FrameState → BSB.Builder)
  → (Word, Word)
  → Fps
  → Seconds
  → Threads
  → IO [Lock]
renderAnimation outDir mkFrameFn (w', h') fps' seconds' threads' =
  let frameRanges = splitRange (0, pred framesTotal') threads'.unThreads in
  CM.forM (zip [1..] frameRanges) $ \(threadN', (framesFrom', framesTo')) → do
    lock <- MVar.newEmptyMVar @()
    let forkF = if isMultiThreaded then CM.void . forkIO else id
    forkF $ do
      renderFramesRangeThread outDir mkFrameFn FS.FrameState
        { w = w'
        , h = h'

        , fps = fps'.unFps
        , seconds = seconds'.unSeconds
        , framesTotal = framesTotal'
        , framesFrom = framesFrom'
        , framesTo = framesTo'

        , threads = threads'.unThreads
        , threadN = threadN'

        , x = minBound
        , y = minBound

        , n = minBound
        }
      CM.when isMultiThreaded $ MVar.putMVar lock ()
    pure . Lock . CM.when isMultiThreaded . MVar.takeMVar $ lock
  where
    framesTotal' = fps'.unFps * seconds'.unSeconds
    isMultiThreaded = threads'.unThreads > 1

renderFramesRangeThread ∷ FilePath → (FS.FrameState → BSB.Builder) → FS.FrameState → IO ()
renderFramesRangeThread outDir mkFrame fs = do
  CM.forM_ [fs.framesFrom .. fs.framesTo] $ \n' → do
    renderFrame outDir mkFrame fs { FS.n = n' }
    CM.when (((n' - fs.framesFrom) `mod` logEveryNFrame) == 0 || n' == fs.framesTo) $
      putStrLn $ unwords
        [ "Thread #" <> show fs.threadN, show (fs.framesFrom, fs.framesTo) <> ":"
        , "Thread frame", show (n' - fs.framesFrom)
        , "of", show (fs.framesTo - fs.framesFrom) <> ","
        , "frame", show n', "of", (show . pred) fs.framesTotal
        , "(last frame, starting from 0,"
        , "" <> (show @Int . ceiling @Float) (fromIntegral (succ n') / fromIntegral fs.fps)
        , "second of", show fs.seconds, "second(s) total)"
        ]
  putStrLn $ "[OK] Thread #" <> show fs.threadN <> " is done"
  where
    logEveryNFrame = 10

-- | File name pattern for a rendered PPM frame file
mkFrameFileName ∷ FS.FrameState → FilePath
mkFrameFileName fs = printf "frame-%09d.ppm" fs.n

-- | Render single frame into a PPM file
renderFrame ∷ FilePath → (FS.FrameState → BSB.Builder) → FS.FrameState → IO ()
renderFrame outDir mkFrame fs = do
  SysIO.withBinaryFile fileName SysIO.WriteMode $ \fh → do
    -- PPM file configuration
    SysIO.hPutStrLn fh "P6"
    SysIO.hPutStrLn fh $ show fs.w <> " " <> show fs.h
    SysIO.hPutStrLn fh "255"

    flip foldMap [0..pred fs.h] $ \y' →
      flip foldMap [0..pred fs.w] $ \x' →
        BSB.hPutBuilder fh $ mkFrame fs { FS.x = x', FS.y = y' }
  where
    fileName = outDir <> "/" <> mkFrameFileName fs
