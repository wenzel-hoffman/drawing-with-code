{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified DrawingWithCode.Animations.XorDevShader as XorDevShader
import qualified DrawingWithCode.Vec as V
import DrawingWithCode.Animation (makeAnimationRenderProgram)

main ∷ IO ()
main =
  makeAnimationRenderProgram "xordev-shader-179-version" $
    V.vecToRgb . XorDevShader.mkXorDevShaderPixel179chars
