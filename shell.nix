let
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    haskell-language-server
    cabal-install
    ghc
  ];
}
