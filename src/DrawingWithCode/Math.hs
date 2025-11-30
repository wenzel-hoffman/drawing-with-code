{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}

module DrawingWithCode.Math
  ( log2
  ) where

log2 ∷ Floating a ⇒ a → a
log2 = logBase 2
{-# INLINE log2 #-}
