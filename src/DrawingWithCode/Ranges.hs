{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DrawingWithCode.Ranges
  ( splitRange
  ) where

-- Example:
--   splitRange (5,25) 4
--   [(5,10),(11,15),(16,20),(21,25)]
splitRange ∷ (Word, Word) → Word → [(Word, Word)]
splitRange (from, to) amountOfRanges =
  filter (uncurry (<=)) $ go from 0 amountOfRanges
  where
    total = to - from + 1
    base = total `div` amountOfRanges
    extra = total `mod` amountOfRanges
    size k = base + (if k < extra then 1 else 0)

    go _ _ 0 = []
    go a k m =
      (a, b) : go (b+1) (k+1) (m-1)
        where
          s = size k
          b = a + s - 1
