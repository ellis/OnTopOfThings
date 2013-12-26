cabal update
cabal install cabal-install
ghc-pkg unregister Cabal

make sure that ~/.cabal/bin is in the path

In this source directory:
cabal sandbox init
cabal install --only-depdencies
cabal configure
cabal build
