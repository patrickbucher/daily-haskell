.PHONY: all clean

all: reversi

reversi: reversi.hs
	ghc -threaded -dynamic -O2 $<

clean:
	rm -f reversi *.hi *.o
