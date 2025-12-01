# Drawing with Code

Drawing visuals/pictures/animations with Haskell code on CPU.

Inspired by https://www.youtube.com/watch?v=xNX9H_ZkfNE

## How to use

Enter [Nix](https://nixos.org/guides/how-nix-works/) shell
(or supply all the dependencies yourself):

``` sh
nix-shell
```

Preview a single (first) frame of a specified animation
(see `executable *` entries in [drawing-with-code.cabal](drawing-with-code.cabal)
file for all available “animations”):

``` sh
ANIMATION=modified-xordev-shader ./view.sh picture save
```

![modified-xordev-shader single frame render](https://raw.githubusercontent.com/wenzel-hoffman/drawing-with-code/7a02e6ea3950dececb06d505383de85526a1db52/renders/modified-xordev-shader-w-600-h-600.png)

Or render and preview 3 seconds of the animation video:

``` sh
ANIMATION=modified-xordev-shader DURATION=3 ./view.sh video
```

You can also render GIF instead of MP4:

``` sh
ANIMATION=modified-xordev-shader DURATION=3 VIDEO_FORMAT=gif ./view.sh video
```

![modified-xordev-shader 3 seconds GIF render](https://github.com/wenzel-hoffman/drawing-with-code/blob/7a02e6ea3950dececb06d505383de85526a1db52/renders/modified-xordev-shader-w-600-h-600-fps-60-dur-3.gif?raw=true)

``` sh
ANIMATION=puker DURATION=3 VIDEO_FORMAT=gif ./view.sh video
```

![puker 3 seconds GIF render](https://github.com/wenzel-hoffman/drawing-with-code/blob/7a02e6ea3950dececb06d505383de85526a1db52/renders/puker-w-600-h-600-fps-60-dur-3.gif?raw=true)
