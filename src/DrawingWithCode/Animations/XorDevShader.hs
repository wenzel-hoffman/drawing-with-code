{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animations.XorDevShader
  ( mkModifiedXorDevShaderPixel
  , mkXorDevShaderPixel195chars
  , mkXorDevShaderPixel179chars
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified DrawingWithCode.Vec as V
import qualified Data.List as List

-- See https://twigl.app?ol=true&ss=-OeSzOONLcUV0fcPofxQ
mkModifiedXorDevShaderPixel ∷ FS.FrameState → V.Vec4
mkModifiedXorDevShaderPixel fs =
  pixel
  where
    g = FS.mkGlsl fs
    p ∷ V.Vec2 = (g.fc * zoomOut * 2 - g.r * zoomOut) / V.vec g.r.y
    l ∷ V.Vec2 = 4.0 - 4.0 * abs (0.7 - V.vec (V.len2 p))
    v ∷ V.Vec2 = p * l

    zoomOut ∷ V.Vec2 = 1.6

    pixel ∷ V.Vec4 =
      let
        reducer (o', v') iy =
          let
            oN = o' + (sin (V.Vec4 w.x w.y w.y w.x) + 3) * (V.vec . abs) (w.x - w.y)
            w = v' + cos (V.Vec2 v'.y v'.x * V.vec iy + V.Vec2 0 iy + V.vec g.t) / V.vec iy + 5
          in
            (oN, w)
        o = fst $ List.foldl' reducer (V.vec 0, v) [1..8]
      in
        tanh (9 * exp (V.vec l.x - 3 - V.vec p.y * V.Vec4 (-3) 0 3 0) / o)

-- See https://x.com/XorDev/status/1894123951401378051
mkXorDevShaderPixel195chars ∷ FS.FrameState → V.Vec4
mkXorDevShaderPixel195chars fs =
  pixel
  where
    g = FS.mkGlsl fs
    p ∷ V.Vec2 = (g.fc * 2 - g.r) / V.vec g.r.y
    l ∷ V.Vec2 = abs (0.7 - V.vec (V.len2 p))
    v ∷ V.Vec2 = p * (1 - l) / 0.2

    pixel ∷ V.Vec4 =
      let
        reducer (o', v') i =
          let
            oN = o' + (sin (V.Vec4 w.x w.y w.y w.x) + 1) * (V.vec . abs) (w.x - w.y) * 0.2
            w = v' + cos (V.Vec2 v'.y v'.x * V.vec i + V.Vec2 0 i + V.vec g.t) / V.vec i + 0.7
          in
            (oN, w)
        o = fst $ List.foldl' reducer (V.vec 0, v) [1..8]
      in
        tanh (exp (V.vec p.y * V.Vec4 1 (-1) (-2) 0) * (exp . negate) (4 * V.vec l.x) / o)

-- See https://twigl.app/?ol=true&ss=-OJyw73KFafCZX9RndXH
mkXorDevShaderPixel179chars ∷ FS.FrameState → V.Vec4
mkXorDevShaderPixel179chars fs =
  pixel
  where
    g = FS.mkGlsl fs
    p ∷ V.Vec2 = (g.fc * 2 - g.r) / V.vec g.r.y
    l ∷ V.Vec2 = 4.0 - 4.0 * abs (0.7 - V.vec (V.len2 p))
    v ∷ V.Vec2 = p * l

    pixel ∷ V.Vec4 =
      let
        reducer (o', v') iy =
          let
            oN = o' + (sin (V.Vec4 w.x w.y w.y w.x) + 1) * (V.vec . abs) (w.x - w.y)
            w = v' + cos (V.Vec2 v'.y v'.x * V.vec iy + V.Vec2 0 iy + V.vec g.t) / V.vec iy + 0.7
          in
            (oN, w)
        o = fst $ List.foldl' reducer (V.vec 0, v) [1..8]
      in
        tanh (5 * exp (V.vec l.x - 4 - V.vec p.y * V.Vec4 (-1) 1 2 0) / o)
