WebExplorer : Index.hs Deform.hs WebExplorer.hs
	ghc -package-db cabal-dev/packages-7.6.3.conf/ -threaded  -Wall  -fwarn-tabs  -funbox-strict-fields  -O2  -fno-warn-unused-do-bind --make -main-is WebExplorer WebExplorer
