{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

import DrawingWithCode.Animation (makeAnimationRenderProgram)
import DrawingWithCode.Animations.ChessBoard (mkChessBoardFrame)

main ∷ IO ()
main =
  makeAnimationRenderProgram "chess-board" mkChessBoardFrame
