{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DrawingWithCode.FrameState
  ( FrameState (..)
  , Glsl (..)
  , mkGlsl
  ) where

import qualified DrawingWithCode.Vec as V

data FrameState = FrameState
  { w ∷ {-# UNPACK #-} !Word
  , h ∷ {-# UNPACK #-} !Word

  , fps ∷ {-# UNPACK #-} !Word
  , seconds ∷ {-# UNPACK #-} !Word
  , framesTotal ∷ {-# UNPACK #-} !Word
  , framesFrom ∷ {-# UNPACK #-} !Word
  , framesTo ∷ {-# UNPACK #-} !Word

  , threads ∷ {-# UNPACK #-} !Word
  , threadN ∷ {-# UNPACK #-} !Word

  , x ∷ {-# UNPACK #-} !Word
  , y ∷ {-# UNPACK #-} !Word

  -- | Frame number starting from 0
  , n ∷ {-# UNPACK #-} !Word
  }
  deriving (Eq, Show)

data Glsl = Glsl
  { r ∷ {-# UNPACK #-} !V.Vec2
  , fc ∷ {-# UNPACK #-} !V.Vec2
  , t ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

mkGlsl ∷ FrameState → Glsl
mkGlsl fs = Glsl
  { r = V.Vec2 (fromIntegral fs.w) (fromIntegral fs.h)
  , fc = V.Vec2 (fromIntegral fs.x) (fromIntegral (fs.h - 1 - fs.y))
  , t = fromIntegral fs.n / fromIntegral fs.fps
  }
{-# INLINE mkGlsl #-}
