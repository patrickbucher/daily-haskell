.PHONY: all clean

all: tictactoe

tictactoe: ex04.hs
	ghc -dynamic -O2 -threaded $< -o $@

clean:
	rm -f *.o *.hi tictactoe
