{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified DrawingWithCode.Vec as V
import DrawingWithCode.Animation (makeAnimationRenderProgram)
import qualified DrawingWithCode.Animations.MandelbrotSet as MandelbrotSet

main ∷ IO ()
main =
  makeAnimationRenderProgram "mandelbrot-set" $
    V.vecToRgb . MandelbrotSet.mkManderbrotSetPixel MandelbrotSet.defaultMandelbrotSetNavigation
