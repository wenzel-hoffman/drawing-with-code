{-# OPTIONS_GHC -Wall -Wno-ambiguous-fields #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DrawingWithCode.Animations.MandelPuker
  ( mkMandelPukerPixel
  ) where

import qualified DrawingWithCode.FrameState as FS
import qualified DrawingWithCode.Vec as V
import Data.Function ((&))
import qualified DrawingWithCode.Animations.MandelbrotSet as MandelbrotSet
import qualified DrawingWithCode.Animations.Puker as Puker

-- A multiplication blend of “mandelbrot-set” and “puker”
mkMandelPukerPixel ∷ FS.FrameState → V.Vec4
mkMandelPukerPixel fs =
  pixel
  where
    mandelPixel ∷ V.Vec3 =
      MandelbrotSet.mkManderbrotSetPixel nav fs { FS.n = 180 }
      where
        nav =
          MandelbrotSet.defaultMandelbrotSetNavigation & \x → x
            { MandelbrotSet.pos = x.pos { V.x = -0.5 }
            , MandelbrotSet.zoom = 3 * (1 - (pukerPixel'.pukerG.t * 0.07))
            }

    m = V.Vec4 (f mandelPixel.x) (f mandelPixel.y) (f mandelPixel.z) 1
      where f = max 0 . min 1

    pukerPixel' ∷ Puker.PukerPixel = Puker.mkPukerPixel fs

    pixel ∷ V.Vec4 =
      pukerPixel'.pukerPixel
        & sqrt
        & sin
        & (** sqrt pukerPixel'.pukerO)
        & (+ V.vec (pukerPixel'.pukerL.x * 0.5))
        & (* m)
        & (* 3)
