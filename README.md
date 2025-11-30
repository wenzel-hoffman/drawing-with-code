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
ANIMATION=modified-xordev-shader ./view.sh picture
```

Or render and preview 3 seconds of the animation video:

``` sh
ANIMATION=modified-xordev-shader DURATION=3 ./view.sh video
```
