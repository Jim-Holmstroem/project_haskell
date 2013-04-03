
all:
	ghc --make -O2 -fforce-recomp -i../Library/ -O answer.hs
	#ghc --make -O2 -rtsopts -threaded -i../Library/ -O answer.hs
clean:
	rm -f *.o *.hi
prof:
	ghc -prof -auto-all -O answer.hs
	./answer +RTS -p
	less answer.prof
run:
	./answer
	#./answer +RTS -N4
