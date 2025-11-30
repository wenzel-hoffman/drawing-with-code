{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animations.ChessBoard
  ( mkChessBoardFrame
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified Data.ByteString.Builder as BSB
import DrawingWithCode.Vec (rgb)

-- See https://www.youtube.com/watch?v=xNX9H_ZkfNE
mkChessBoardFrame ∷ FS.FrameState → BSB.Builder
mkChessBoardFrame fs =
  if even (div (fs.x + fs.n) 60 + div (fs.y + fs.n) 60)
    then rgb (255,255,255)
    else rgb (0,0,0)
