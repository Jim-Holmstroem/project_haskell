all:
	ghc --make -O2 -fforce-recomp -i../Library/ -O answer.hs
	#ghc --make -O2 -rtsopts -threaded -i../Library/ -O answer.hs
clean:
	rm -f *.o *.hi
run:
	./answer
	#./answer +RTS -N4
