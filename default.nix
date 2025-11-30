{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib

, inNixShell ? false # Set to `true` automatically when used with nix-shell
}:

let
  hsPkgs = pkgs.haskellPackages.extend (self: super: {
    drawing-with-code = makeHaskellPackage ./.;
  });

  makeHaskellPackage = dir:
    let
      name = baseNameOf (toString dir);

      withoutCabalDistDir = fileName: fileType: ! (
        fileType == "directory" &&
        builtins.match "^dist(-newstyle)?$" (baseNameOf fileName) != null
      );

      filter = fileName: fileType:
        withoutCabalDistDir fileName fileType &&
        lib.cleanSourceFilter fileName fileType;

      cleanSrc =
        pkgs.nix-gitignore.gitignoreFilterRecursiveSource
          filter
          [ ./.gitignore ]
          dir;
    in
      assert builtins.isPath dir;
      hsPkgs.callCabal2nix name cleanSrc {};

  shell = hsPkgs.shellFor {
    packages = p: [ p.drawing-with-code ];
    withHoogle = true;

    buildInputs = [
      hsPkgs.haskell-language-server
      hsPkgs.hlint
      hsPkgs.cabal-install

      pkgs.shellcheck

      pkgs.ffmpeg # For rendering videos from generated *.ppm frame files
      pkgs.mpv # For previewing rendered videos
      pkgs.feh # For previewing frames

      pkgs.coreutils
    ];
  };
in

(if inNixShell then shell else {}) // {
  inherit shell;

  drawing-with-code =
    pkgs.haskell.lib.justStaticExecutables hsPkgs.drawing-with-code;
}
