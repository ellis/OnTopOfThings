sudo apt-get install cabal-install
cabal update
cabal install cabal-install
ghc-pkg unregister Cabal

make sure that ~/.cabal/bin is in the path

In this source directory:
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
