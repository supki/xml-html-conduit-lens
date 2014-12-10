{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "xml-html-conduit-lens";
  version = "0.3.2.1";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  buildDepends = with haskellPackages; [ htmlConduit lens text xmlConduit ];
  testDepends = with haskellPackages; buildDepends ++ [
    doctest hspec hspecExpectationsLens
  ];
  meta = {
    homepage = "https://github.com/supki/xml-html-conduit-lens#readme";
    description = "Optics for xml-conduit and html-conduit";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
