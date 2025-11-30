{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animations.Puker
  ( mkPukerPixel
  , PukerPixel (..)
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified DrawingWithCode.Vec as V
import qualified Data.List as List
import Data.Function ((&))
import DrawingWithCode.Math (log2)

-- See https://twigl.app/?ol=true&ss=-OeTznc5-TW9jrlEpJ6e
mkPukerPixel ∷ FS.FrameState → PukerPixel
mkPukerPixel fs =
  PukerPixel
    { pukerPixel = pixel
    , pukerG = g
    , pukerP = p
    , pukerL = l
    , pukerV = v
    , pukerO = o
    }
  where
    g = FS.mkGlsl fs
    p ∷ V.Vec2 = (g.fc * zoomOut * 2 - g.r * zoomOut) / V.vec g.r.y
    l ∷ V.Vec2 = V.vec $ tan 4.0 - 4.0 * abs (0.7 - atan (V.len2 p))
    v ∷ V.Vec2 = p * l

    zoomOut ∷ V.Vec2 = 2

    pixel ∷ V.Vec4 =
      atan (9 * exp (V.vec l.x - 3 - V.vec p.y * V.Vec4 (-3) 0 3 0) / sqrt o * 0.5)

    o ∷ V.Vec4 =
      let
        reducer (o', v') iy =
          let
            oN = o' + (tanh (V.Vec4 w.x w.y w.y w.x) + 3) * (abs . sin . tan . V.vec) (w.x - w.y)
            w
              = v'
              & (\x → x +
                  ( cos (sin (V.Vec2 x.y x.x) * V.vec iy + tan (V.Vec2 0 iy) + V.vec g.t)
                  / V.vec iy + 5
                  )
                )
              & (* 3.0)
              & (\x → x + (sqrt . sqrt) (log2 x * (atan . log2) x))
          in
            (oN, w)
      in
        fst $ List.foldl' reducer (V.vec 0, v) [1..2]

data PukerPixel = PukerPixel
  { pukerPixel ∷ {-# UNPACK #-} !V.Vec4
  , pukerG ∷ {-# UNPACK #-} !FS.Glsl
  , pukerP ∷ {-# UNPACK #-} !V.Vec2
  , pukerL ∷ {-# UNPACK #-} !V.Vec2
  , pukerV ∷ {-# UNPACK #-} !V.Vec2
  , pukerO ∷ {-# UNPACK #-} !V.Vec4
  }
  deriving (Eq, Show)
