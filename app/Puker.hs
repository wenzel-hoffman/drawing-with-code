{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified DrawingWithCode.Vec as V
import DrawingWithCode.Animation (makeAnimationRenderProgram)
import qualified DrawingWithCode.Animations.Puker as Puker

main ∷ IO ()
main =
  makeAnimationRenderProgram "puker" $
    V.vecToRgb . (\x → x.pukerPixel) . Puker.mkPukerPixel
