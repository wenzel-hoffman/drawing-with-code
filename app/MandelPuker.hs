{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified DrawingWithCode.Vec as V
import DrawingWithCode.Animation (makeAnimationRenderProgram)
import qualified DrawingWithCode.Animations.MandelPuker as MandelPuker

main ∷ IO ()
main =
  makeAnimationRenderProgram "mandel-puker" $
    V.vecToRgb . MandelPuker.mkMandelPukerPixel
