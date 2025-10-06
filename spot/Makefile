.PHONY: all clean

all: spot

spot: spot.hs
	ghc -threaded -dynamic -O2 $<

clean:
	rm -f spot *.hi *.o
