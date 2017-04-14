all:
	@echo "Building..."
#	@ghc --make AvazPred.hs
#	@ghc --make AvazInteract.hs
	@ghc -with-rtsopts="-K1G -N4" -threaded -O3 --make AvazInteractVty.hs
	@ghc -with-rtsopts="-K1G -N4" -threaded -O3 --make -L/usr/lib/ -lstdc++ AvazInteractGui.hs
#/Users/malar/Documents/Devel/Avaz/cpp/libfreqstem.0.dylib 
	@ghc --make HsUlapi.hs

clean:
	@echo "Cleaning..."
	@rm -f *.hi *.o AvazPred AvazInteract AvazInteractVty AvazInteractGui

