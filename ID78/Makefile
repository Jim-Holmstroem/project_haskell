all:
	ghc -rtsopts --make -O2 -i../Library/ -O answer.hs
clean:
	rm -f *.o *.hi
prof:
	ghc -prof -auto-all -O answer.hs
	./answer +RTS -p
	less answer.prof
run:
	./answer +RTS  -RTS
