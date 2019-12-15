let

  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/54f385241e6649128ba963c10314942d73245479";
    sha256 = "0bd4v8v4xcdbaiaa59yqprnc6dkb9jv12mb0h5xz7b51687ygh9l";
  }) {
    config = {};
    overlays = [];
  };
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      distr = self.callCabal2nix "distr" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^.*\\.cabal$"
      ]) {};
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.distr ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.distr // {
  inherit env hpkgs;
}
