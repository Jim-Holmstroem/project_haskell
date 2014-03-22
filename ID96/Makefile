all:
	ghc --make -rtsopts -threaded  -i../Library/ -O answer.hs
op:
	ghc --make -O2 -rtsopts -threaded  -i../Library/ -O answer.hs
clean:
	rm -f *.o *.hi
run:
	./answer +RTS -N4 < sudoku.txt
