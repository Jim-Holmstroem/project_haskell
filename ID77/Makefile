all:
	ghc -threaded --make -i../Library/ -O answer.hs
clean:
	rm -f *.o *.hi
run:
	./answer +RTS -N4 -RTS
