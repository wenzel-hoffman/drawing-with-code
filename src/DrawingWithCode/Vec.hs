{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DrawingWithCode.Vec
  ( Vec2 (..)
  , Vec3 (..)
  , Vec4 (..)
  , Vec (..)
  , Dot (..)
  , len2
  , len
  , VecToRgb (..)
  , rgb
  ) where

import qualified Data.ByteString.Builder as BSB
import Data.Word (Word8)

-- * Vec2

data Vec2 = Vec2
  { x ∷ {-# UNPACK #-} !Float
  , y ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

instance Num Vec2 where
  Vec2 x1 y1 + Vec2 x2 y2 = Vec2 (x1 + x2) (y1 + y2)
  {-# INLINE (+) #-}
  Vec2 x1 y1 - Vec2 x2 y2 = Vec2 (x1 - x2) (y1 - y2)
  {-# INLINE (-) #-}
  Vec2 x1 y1 * Vec2 x2 y2 = Vec2 (x1 * x2) (y1 * y2)
  {-# INLINE (*) #-}
  abs (Vec2 x y) = Vec2 (abs x) (abs y)
  {-# INLINE abs #-}
  signum (Vec2 x y) = Vec2 (signum x) (signum y)
  {-# INLINE signum #-}
  fromInteger n = let a = fromInteger n in Vec2 a a
  {-# INLINE fromInteger #-}

instance Fractional Vec2 where
  Vec2 x1 y1 / Vec2 x2 y2 = Vec2 (x1 / x2) (y1 / y2)
  {-# INLINE (/) #-}
  recip (Vec2 x y) = Vec2 (recip x) (recip y)
  {-# INLINE recip #-}
  fromRational r = let a = fromRational r in Vec2 a a
  {-# INLINE fromRational #-}

instance Floating Vec2 where
  pi = Vec2 pi pi
  {-# INLINE pi #-}
  exp (Vec2 x y) = Vec2 (exp x) (exp y)
  {-# INLINE exp #-}
  log (Vec2 x y) = Vec2 (log x) (log y)
  {-# INLINE log #-}
  sin (Vec2 x y) = Vec2 (sin x) (sin y)
  {-# INLINE sin #-}
  cos (Vec2 x y) = Vec2 (cos x) (cos y)
  {-# INLINE cos #-}
  tan (Vec2 x y) = Vec2 (tan x) (tan y)
  {-# INLINE tan #-}
  asin (Vec2 x y) = Vec2 (asin x) (asin y)
  {-# INLINE asin #-}
  acos (Vec2 x y) = Vec2 (acos x) (acos y)
  {-# INLINE acos #-}
  atan (Vec2 x y) = Vec2 (atan x) (atan y)
  {-# INLINE atan #-}
  sinh (Vec2 x y) = Vec2 (sinh x) (sinh y)
  {-# INLINE sinh #-}
  cosh (Vec2 x y) = Vec2 (cosh x) (cosh y)
  {-# INLINE cosh #-}
  tanh (Vec2 x y) = Vec2 (tanh x) (tanh y)
  {-# INLINE tanh #-}
  asinh (Vec2 x y) = Vec2 (asinh x) (asinh y)
  {-# INLINE asinh #-}
  acosh (Vec2 x y) = Vec2 (acosh x) (acosh y)
  {-# INLINE acosh #-}
  atanh (Vec2 x y) = Vec2 (atanh x) (atanh y)
  {-# INLINE atanh #-}

instance Dot Vec2 where
  dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2
  {-# INLINE dot #-}

instance Vec Vec2 where
  vec x = Vec2 x x
  {-# INLINE vec #-}

-- * Vec3

data Vec3 = Vec3
  { x ∷ {-# UNPACK #-} !Float
  , y ∷ {-# UNPACK #-} !Float
  , z ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

instance Num Vec3 where
  Vec3 x1 y1 z1 + Vec3 x2 y2 z2 = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  {-# INLINE (+) #-}
  Vec3 x1 y1 z1 - Vec3 x2 y2 z2 = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  {-# INLINE (-) #-}
  Vec3 x1 y1 z1 * Vec3 x2 y2 z2 = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  {-# INLINE (*) #-}
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  {-# INLINE abs #-}
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  {-# INLINE signum #-}
  fromInteger n = let a = fromInteger n in Vec3 a a a
  {-# INLINE fromInteger #-}

instance Fractional Vec3 where
  Vec3 x1 y1 z1 / Vec3 x2 y2 z2 = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)
  {-# INLINE (/) #-}
  recip (Vec3 x y z) = Vec3 (recip x) (recip y) (recip z)
  {-# INLINE recip #-}
  fromRational r = let a = fromRational r in Vec3 a a a
  {-# INLINE fromRational #-}

instance Floating Vec3 where
  pi = vec pi
  {-# INLINE pi #-}
  exp (Vec3 x y z) = Vec3 (exp x) (exp y) (exp z)
  {-# INLINE exp #-}
  log (Vec3 x y z) = Vec3 (log x) (log y) (log z)
  {-# INLINE log #-}
  sin (Vec3 x y z) = Vec3 (sin x) (sin y) (sin z)
  {-# INLINE sin #-}
  cos (Vec3 x y z) = Vec3 (cos x) (cos y) (cos z)
  {-# INLINE cos #-}
  tan (Vec3 x y z) = Vec3 (tan x) (tan y) (tan z)
  {-# INLINE tan #-}
  asin (Vec3 x y z) = Vec3 (asin x) (asin y) (asin z)
  {-# INLINE asin #-}
  acos (Vec3 x y z) = Vec3 (acos x) (acos y) (acos z)
  {-# INLINE acos #-}
  atan (Vec3 x y z) = Vec3 (atan x) (atan y) (atan z)
  {-# INLINE atan #-}
  sinh (Vec3 x y z) = Vec3 (sinh x) (sinh y) (sinh z)
  {-# INLINE sinh #-}
  cosh (Vec3 x y z) = Vec3 (cosh x) (cosh y) (cosh z)
  {-# INLINE cosh #-}
  tanh (Vec3 x y z) = Vec3 (tanh x) (tanh y) (tanh z)
  {-# INLINE tanh #-}
  asinh (Vec3 x y z) = Vec3 (asinh x) (asinh y) (asinh z)
  {-# INLINE asinh #-}
  acosh (Vec3 x y z) = Vec3 (acosh x) (acosh y) (acosh z)
  {-# INLINE acosh #-}
  atanh (Vec3 x y z) = Vec3 (atanh x) (atanh y) (atanh z)
  {-# INLINE atanh #-}

instance Dot Vec3 where
  dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  {-# INLINE dot #-}

instance Vec Vec3 where
  vec x = Vec3 x x x
  {-# INLINE vec #-}

-- * Vec4

data Vec4 = Vec4
  { x ∷ {-# UNPACK #-} !Float
  , y ∷ {-# UNPACK #-} !Float
  , z ∷ {-# UNPACK #-} !Float
  , w ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

instance Num Vec4 where
  Vec4 x1 y1 z1 w1 + Vec4 x2 y2 z2 w2 = Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  {-# INLINE (+) #-}
  Vec4 x1 y1 z1 w1 - Vec4 x2 y2 z2 w2 = Vec4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  {-# INLINE (-) #-}
  Vec4 x1 y1 z1 w1 * Vec4 x2 y2 z2 w2 = Vec4 (x1 * x2) (y1 * y2) (z1 * z2) (w1 * w2)
  {-# INLINE (*) #-}
  abs (Vec4 x y z w) = Vec4 (abs x) (abs y) (abs z) (abs w)
  {-# INLINE abs #-}
  signum (Vec4 x y z w) = Vec4 (signum x) (signum y) (signum z) (signum w)
  {-# INLINE signum #-}
  fromInteger n = let a = fromInteger n in Vec4 a a a a
  {-# INLINE fromInteger #-}

instance Fractional Vec4 where
  Vec4 x1 y1 z1 w1 / Vec4 x2 y2 z2 w2 = Vec4 (x1 / x2) (y1 / y2) (z1 / z2) (w1 / w2)
  {-# INLINE (/) #-}
  recip (Vec4 x y z w) = Vec4 (recip x) (recip y) (recip z) (recip w)
  {-# INLINE recip #-}
  fromRational r = let a = fromRational r in Vec4 a a a a
  {-# INLINE fromRational #-}

instance Floating Vec4 where
  pi = Vec4 pi pi pi pi
  {-# INLINE pi #-}
  exp (Vec4 x y z w) = Vec4 (exp x) (exp y) (exp z) (exp w)
  {-# INLINE exp #-}
  log (Vec4 x y z w) = Vec4 (log x) (log y) (log z) (log w)
  {-# INLINE log #-}
  sin (Vec4 x y z w) = Vec4 (sin x) (sin y) (sin z) (sin w)
  {-# INLINE sin #-}
  cos (Vec4 x y z w) = Vec4 (cos x) (cos y) (cos z) (cos w)
  {-# INLINE cos #-}
  tan (Vec4 x y z w) = Vec4 (tan x) (tan y) (tan z) (tan w)
  {-# INLINE tan #-}
  asin (Vec4 x y z w) = Vec4 (asin x) (asin y) (asin z) (asin w)
  {-# INLINE asin #-}
  acos (Vec4 x y z w) = Vec4 (acos x) (acos y) (acos z) (acos w)
  {-# INLINE acos #-}
  atan (Vec4 x y z w) = Vec4 (atan x) (atan y) (atan z) (atan w)
  {-# INLINE atan #-}
  sinh (Vec4 x y z w) = Vec4 (sinh x) (sinh y) (sinh z) (sinh w)
  {-# INLINE sinh #-}
  cosh (Vec4 x y z w) = Vec4 (cosh x) (cosh y) (cosh z) (cosh w)
  {-# INLINE cosh #-}
  tanh (Vec4 x y z w) = Vec4 (tanh x) (tanh y) (tanh z) (tanh w)
  {-# INLINE tanh #-}
  asinh (Vec4 x y z w) = Vec4 (asinh x) (asinh y) (asinh z) (asinh w)
  {-# INLINE asinh #-}
  acosh (Vec4 x y z w) = Vec4 (acosh x) (acosh y) (acosh z) (acosh w)
  {-# INLINE acosh #-}
  atanh (Vec4 x y z w) = Vec4 (atanh x) (atanh y) (atanh z) (atanh w)
  {-# INLINE atanh #-}

instance Dot Vec4 where
  dot (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
  {-# INLINE dot #-}

instance Vec Vec4 where
  vec x = Vec4 x x x x
  {-# INLINE vec #-}

-- * Vec

-- | Make @VecN@ out of a @Float@
class Vec v where
  vec ∷ Float → v

instance Vec Float where
  vec = id
  {-# INLINE vec #-}

-- * Dot

class Dot v where
  dot ∷ v → v → Float

len2 ∷ Dot v ⇒ v → Float
len2 p = dot p p
{-# INLINE len2 #-}

len ∷ Dot v ⇒ v → Float
len p = sqrt (len2 p)
{-# INLINE len #-}

-- * VecToRgb

-- | Convert a @VecN@ to a PPM format RGB pixel
class VecToRgb v where
  vecToRgb ∷ v → BSB.Builder

instance VecToRgb Vec3 where
  vecToRgb v =
    rgb
      ( floor ((max 0 . min 1) v.x * 255)
      , floor ((max 0 . min 1) v.y * 255)
      , floor ((max 0 . min 1) v.z * 255)
      )
  {-# INLINE vecToRgb #-}

instance VecToRgb Vec4 where
  vecToRgb v =
    rgb
      ( floor ((max 0 . min 1) v.x * 255)
      , floor ((max 0 . min 1) v.y * 255)
      , floor ((max 0 . min 1) v.z * 255)
      )
  {-# INLINE vecToRgb #-}

rgb ∷ (Word8, Word8, Word8) → BSB.Builder
rgb (r,g,b) = BSB.word8 r <> BSB.word8 g <> BSB.word8 b
{-# INLINE rgb #-}
