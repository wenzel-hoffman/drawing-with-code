{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animations.MandelbrotSet
  ( mkManderbrotSetPixel
  , MandelbrotSetNavigation (..)
  , defaultMandelbrotSetNavigation
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified DrawingWithCode.Vec as V

mkManderbrotSetPixel ∷ MandelbrotSetNavigation → FS.FrameState → V.Vec3
mkManderbrotSetPixel nav fs =
  pixel
  where
    g = FS.mkGlsl fs

    pixel ∷ V.Vec3 =
      V.Vec3 (sqrt (fract x) / 3) (fract x) (sqrt (fract x))
      where
        rx ∷ Float = V.vec $ g.r.x / g.r.y
        ry ∷ Float = V.vec $ g.r.y / g.r.x

        correctedSize = V.Vec2 (g.r.x * min ry 1) (g.r.y * min rx 1)
        centering = V.Vec2 (1 - max rx 1) (1 - max ry 1)

        position ∷ V.Vec2
          = ((g.fc / correctedSize * 2) + centering - 1)
          * 2 -- Convert canvas to range from -2.0 to +2.0
          / V.vec nav.zoom
          + nav.pos

        x = mandelbrotSet position

    mandelbrotSet (point ∷ V.Vec2) =
      go 0 point
      where
        threshold ∷ Float = 16
        detalizationSpeed ∷ Float = 30 -- Hz or FPS (added iterations per second)
        maxIterations ∷ Int = 1000
        limit ∷ Int = min (round (g.t * detalizationSpeed)) maxIterations

        go (i ∷ Int) (xy ∷ V.Vec2)
          | i >= limit = 1 ∷ Float
          | otherwise =
              let
                v = V.Vec2 (xy.x * xy.x - xy.y * xy.y) (2 * xy.x * xy.y)
              in
                if abs (v.x + v.y) > threshold
                  then fromIntegral i / fromIntegral limit
                  else go (succ i) (v + point)

    fract ∷ Float → Float
    fract x = x - fromIntegral (floor x ∷ Int)
    {-# INLINE fract #-}

-- For navigating around the set
data MandelbrotSetNavigation = MandelbrotSetNavigation
  { pos ∷ {-# UNPACK #-} !V.Vec2
  , zoom ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

defaultMandelbrotSetNavigation ∷ MandelbrotSetNavigation
defaultMandelbrotSetNavigation = MandelbrotSetNavigation
  { pos = V.Vec2 0 0
  , zoom = 1
  }
