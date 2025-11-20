#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -Wall -Wno-ambiguous-fields -threaded #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

import qualified System.Environment as SysEnv
import qualified System.IO as SysIO
import Text.Printf (printf)
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word8)
import qualified Control.Monad as CM
import qualified Data.List as List
import Text.Read (readEither)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent (forkIO)

main ∷ IO ()
main = do
  (outDir, animationF, w', h', fps', seconds', threads') ←
    SysEnv.getArgs >>= \case
      [outDir, animation, w', h', fps', seconds', threads'] → do
        animationF ← do
          case List.find (\(x, _) → x == animation) animations of
            Nothing → argsFailure $ "Unrecognized animation: " <> animation
            Just (_, f) → pure f
        w'' ← parseWord "WIDTH" w'
        h'' ← parseWord "HEIGHT" h'
        fps'' ← parseWord "FPS" fps'
        seconds'' ← parseWord "SECONDS" seconds'
        threads'' ← parseWord "THREADS" threads'
        pure (outDir, animationF, w'', h'', fps'', seconds'', threads'')
      args → argsFailure $ "Unexpected arguments: " <> show args

  let framesTotal' = fps' * seconds'

  locks ←
    let frameRanges = splitRange (0, pred framesTotal') threads' in
    CM.forM (zip [1..] frameRanges) $ \(threadN', (framesFrom', framesTo')) → do
      lock <- MVar.newEmptyMVar @()
      (lock <$) . forkIO $ do
        renderAnimationThread outDir animationF FrameState
          { w = w'
          , h = h'

          , fps = fps'
          , seconds = seconds'
          , framesTotal = framesTotal'
          , framesFrom = framesFrom'
          , framesTo = framesTo'

          , threads = threads'
          , threadN = threadN'

          , x = minBound
          , y = minBound

          , n = minBound
          }
        MVar.putMVar lock ()

  CM.forM_ locks MVar.takeMVar
  putStrLn $ "[OK] All " <> show threads' <> " thread(s) are done"

  where
    parseWord placeholder
      = either (argsFailure . (("Can’t parse " <> placeholder <> ": ") <>) . show) pure
      . readEither @Word

    argsFailure errMsg =
      fail $ unlines
        [ "\n\n" <> errMsg <> "\n"
        , "Usage: ./generate.hs OUTPUT_DIR ANIMATION WIDTH HEIGHT FPS SECONDS THREADS\n"
        , "Usage example: ./generate.hs /tmp/foo xordev-shader-179-version 600 600 60 10 4\n"
        , "All available ANIMATIONs:"
        , foldMap (\(x, _) → "  - " <> x <> "\n") animations
        ]

animations ∷ [(String, FrameState → BSB.Builder)]
animations =
  [ ("mandelbrot-set", mkManderbrotSetFrame)
  , ("modified-xordev-shader", mkModifiedXorDevShaderFrame)
  , ("xordev-shader-179-version", mkXorDevShaderFrame179chars)
  , ("xordev-shader-195-version", mkXorDevShaderFrame195chars)
  , ("chess-board", mkChessBoardFrame)
  ]

mkManderbrotSetFrame ∷ FrameState → BSB.Builder
mkManderbrotSetFrame fs =
  vecToRgb pixel
  where
    g = mkGlsl fs

    -- For navigating around the set
    nav = Vec2 0 0
    zoom ∷ Float = 1

    pixel ∷ Vec3 =
      Vec3 (sqrt (fract x) / 3) (fract x) (sqrt (fract x))
      where
        rx ∷ Float = vec $ g.r.x / g.r.y
        ry ∷ Float = vec $ g.r.y / g.r.x

        correctedSize = Vec2 (g.r.x * min ry 1) (g.r.y * min rx 1)
        centering = Vec2 (1 - max rx 1) (1 - max ry 1)

        position ∷ Vec2
          = ((g.fc / correctedSize * 2) + centering - 1)
          * 2 -- Convert canvas to range from -2.0 to +2.0
          / vec zoom
          + nav

        x = mandelbrotSet position

    mandelbrotSet (point ∷ Vec2) =
      go 0 point
      where
        threshold ∷ Float = 16
        detalizationSpeed ∷ Float = 30 -- Hz or FPS (added iterations per second)
        maxIterations ∷ Int = 1000
        limit ∷ Int = min (round (g.t * detalizationSpeed)) maxIterations

        go (i ∷ Int) (xy ∷ Vec2)
          | i >= limit = 1 ∷ Float
          | otherwise =
              let
                v = Vec2 (xy.x * xy.x - xy.y * xy.y) (2 * xy.x * xy.y)
              in
                if abs (v.x + v.y) > threshold
                  then fromIntegral i / fromIntegral limit
                  else go (succ i) (v + point)

    fract ∷ Float → Float
    fract x = x - fromIntegral (floor x ∷ Int)
    {-# INLINE fract #-}

-- See https://www.youtube.com/watch?v=xNX9H_ZkfNE
mkChessBoardFrame ∷ FrameState → BSB.Builder
mkChessBoardFrame fs =
  if even (div (fs.x + fs.n) 60 + div (fs.y + fs.n) 60)
    then rgb (255,255,255)
    else rgb (0,0,0)

-- See https://twigl.app?ol=true&ss=-OeSzOONLcUV0fcPofxQ
mkModifiedXorDevShaderFrame ∷ FrameState → BSB.Builder
mkModifiedXorDevShaderFrame fs =
  vecToRgb pixel
  where
    g = mkGlsl fs
    p ∷ Vec2 = (g.fc * zoomOut * 2 - g.r * zoomOut) / vec g.r.y
    l ∷ Vec2 = 4.0 - 4.0 * abs (0.7 - vec (len2 p))
    v ∷ Vec2 = p * l

    zoomOut ∷ Vec2 = 1.6

    pixel ∷ Vec4 =
      let
        reducer (o', v') iy =
          let
            oN = o' + (sin (Vec4 w.x w.y w.y w.x) + 3) * (vec . abs) (w.x - w.y)
            w = v' + cos (Vec2 v'.y v'.x * vec iy + Vec2 0 iy + vec g.t) / vec iy + 5
          in
            (oN, w)
        o = fst $ List.foldl' reducer (vec 0, v) [1..8]
      in
        tanh (9 * exp (vec l.x - 3 - vec p.y * Vec4 (-3) 0 3 0) / o)

-- See https://x.com/XorDev/status/1894123951401378051
mkXorDevShaderFrame195chars ∷ FrameState → BSB.Builder
mkXorDevShaderFrame195chars fs =
  vecToRgb pixel
  where
    g = mkGlsl fs
    p ∷ Vec2 = (g.fc * 2 - g.r) / vec g.r.y
    l ∷ Vec2 = abs (0.7 - vec (len2 p))
    v ∷ Vec2 = p * (1 - l) / 0.2

    pixel ∷ Vec4 =
      let
        reducer (o', v') i =
          let
            oN = o' + (sin (Vec4 w.x w.y w.y w.x) + 1) * (vec . abs) (w.x - w.y) * 0.2
            w = v' + cos (Vec2 v'.y v'.x * vec i + Vec2 0 i + vec g.t) / vec i + 0.7
          in
            (oN, w)
        o = fst $ List.foldl' reducer (vec 0, v) [1..8]
      in
        tanh (exp (vec p.y * Vec4 1 (-1) (-2) 0) * (exp . negate) (4 * vec l.x) / o)

-- See https://twigl.app/?ol=true&ss=-OJyw73KFafCZX9RndXH
mkXorDevShaderFrame179chars ∷ FrameState → BSB.Builder
mkXorDevShaderFrame179chars fs =
  vecToRgb pixel
  where
    g = mkGlsl fs
    p ∷ Vec2 = (g.fc * 2 - g.r) / vec g.r.y
    l ∷ Vec2 = 4.0 - 4.0 * abs (0.7 - vec (len2 p))
    v ∷ Vec2 = p * l

    pixel ∷ Vec4 =
      let
        reducer (o', v') iy =
          let
            oN = o' + (sin (Vec4 w.x w.y w.y w.x) + 1) * (vec . abs) (w.x - w.y)
            w = v' + cos (Vec2 v'.y v'.x * vec iy + Vec2 0 iy + vec g.t) / vec iy + 0.7
          in
            (oN, w)
        o = fst $ List.foldl' reducer (vec 0, v) [1..8]
      in
        tanh (5 * exp (vec l.x - 4 - vec p.y * Vec4 (-1) 1 2 0) / o)

renderAnimationThread ∷ FilePath → (FrameState → BSB.Builder) → FrameState → IO ()
renderAnimationThread outDir mkFrame fs = do
  CM.forM_ [fs.framesFrom .. fs.framesTo] $ \n' → do
    renderFrame outDir mkFrame fs { n = n' }
    CM.when ((n' `mod` logEveryNFrame) == 0 || n' == fs.framesTo) $
      putStrLn $ unwords
        [ "Thread #" <> show fs.threadN, show (fs.framesFrom, fs.framesTo) <> ":"
        , "Thread frame", show (n' - fs.framesFrom)
        , "of", show (fs.framesTo - fs.framesFrom) <> ","
        , "frame", show n', "of", show fs.framesTotal, "frames total"
        , "(" <> (show @Int . ceiling @Float) (fromIntegral (succ n') / fromIntegral fs.fps)
        , "second of", show fs.seconds, "second(s) total)"
        ]
  putStrLn $ "[OK] Thread #" <> show fs.threadN <> " is done"
  where
    logEveryNFrame = 10

mkFrameFileName ∷ FrameState → FilePath
mkFrameFileName fs = printf "frame-%09d.ppm" fs.n

renderFrame ∷ FilePath → (FrameState → BSB.Builder) → FrameState → IO ()
renderFrame outDir mkFrame fs = do
  SysIO.withBinaryFile fileName SysIO.WriteMode $ \fh → do
    SysIO.hPutStrLn fh "P6"
    SysIO.hPutStrLn fh $ show fs.w <> " " <> show fs.h
    SysIO.hPutStrLn fh "255"
    flip foldMap [0..pred fs.h] $ \y' →
      flip foldMap [0..pred fs.w] $ \x' →
        BSB.hPutBuilder fh $ mkFrame fs { x = x', y = y' }
  where
    fileName = outDir <> "/" <> mkFrameFileName fs

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

data FrameState = FrameState
  { w ∷ {-# UNPACK #-} !Word
  , h ∷ {-# UNPACK #-} !Word

  , fps ∷ {-# UNPACK #-} !Word
  , seconds ∷ {-# UNPACK #-} !Word
  , framesTotal ∷ {-# UNPACK #-} !Word
  , framesFrom ∷ {-# UNPACK #-} !Word
  , framesTo ∷ {-# UNPACK #-} !Word

  , threads ∷ {-# UNPACK #-} !Word
  , threadN ∷ {-# UNPACK #-} !Word

  , x ∷ {-# UNPACK #-} !Word
  , y ∷ {-# UNPACK #-} !Word

  -- | Frame number starting from 0
  , n ∷ {-# UNPACK #-} !Word
  }
  deriving (Eq, Show)

class Dot v where
  dot ∷ v → v → Float

len2 ∷ Dot v ⇒ v → Float
len2 p = dot p p
{-# INLINE len2 #-}

-- len ∷ Dot v ⇒ v → Float
-- len p = sqrt (len2 p)
-- {-# INLINE len #-}

class Vec v where
  vec ∷ Float → v
instance Vec Float where
  vec = id
  {-# INLINE vec #-}

rgb ∷ (Word8, Word8, Word8) → BSB.Builder
rgb (r,g,b) = BSB.word8 r <> BSB.word8 g <> BSB.word8 b
{-# INLINE rgb #-}

class VecToRgb v where
  vecToRgb ∷ v → BSB.Builder
instance VecToRgb Vec3 where
  vecToRgb v =
    rgb (floor (v.x * 255), floor (v.y * 255), floor (v.z * 255))
  {-# INLINE vecToRgb #-}
instance VecToRgb Vec4 where
  vecToRgb v =
    rgb (floor (v.x * 255), floor (v.y * 255), floor (v.z * 255))
  {-# INLINE vecToRgb #-}

data Glsl = Glsl
  { r ∷ {-# UNPACK #-} !Vec2
  , fc ∷ {-# UNPACK #-} !Vec2
  , t ∷ {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

mkGlsl ∷ FrameState → Glsl
mkGlsl fs = Glsl
  { r = Vec2 (fromIntegral fs.w) (fromIntegral fs.h)
  , fc = Vec2 (fromIntegral fs.x) (fromIntegral (fs.h - 1 - fs.y))
  , t = fromIntegral fs.n / fromIntegral fs.fps
  }
{-# INLINE mkGlsl #-}

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
