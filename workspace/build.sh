rm -rf dist-newstyle
cd ..; ghc Gen.hs ; cd workspace
../Gen
cabal new-build harmadillo
cabal new-exec -- ghc ex1.hs
