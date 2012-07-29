
all:
	echo "Recursive make; not implemented yet."

clean:
	@echo "Removing.."
	-find -name *.o
	-rm `find -name *.o`
	@echo "Removing.."
	-find -name *.hi
	-rm `find -name *.hi`

