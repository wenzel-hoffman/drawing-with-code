{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module DrawingWithCode.ArgsParse
  ( parseArgs
  ) where

import Text.Read (readEither)
import qualified System.Environment as SysEnv
import qualified DrawingWithCode.Render as Render

parseArgs ∷ String → IO (FilePath, (Word, Word), Render.Fps, Render.Seconds, Render.Threads)
parseArgs exeName =
  SysEnv.getArgs >>= \case
    [outDir, w', h', fps', seconds', threads'] → do
      w'' ← parseWord "WIDTH" w'
      h'' ← parseWord "HEIGHT" h'
      fps'' ← Render.Fps <$> parseWord "FPS" fps'
      seconds'' ← Render.Seconds <$> parseWord "SECONDS" seconds'
      threads'' ← Render.Threads <$> parseWord "THREADS" threads'
      pure (outDir, (w'', h''), fps'', seconds'', threads'')
    args → argsFailure $ "Unexpected arguments: " <> show args
  where
    parseWord placeholder
      = either (argsFailure . (("Can’t parse " <> placeholder <> ": ") <>) . show) pure
      . readEither @Word

    argsFailure errMsg =
      fail $ unlines
        [ "\n\n" <> errMsg <> "\n"
        , "Usage: " <> exeName <> " OUTPUT_DIR WIDTH HEIGHT FPS SECONDS THREADS\n"
        , "Usage example: " <> exeName <> " /tmp/foo 600 600 60 10 4\n"
        ]
