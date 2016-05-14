# Makefile for TransactChat

HC   = ghc
SRC  = src/
BIN  = bin/
ARGS = -i$(SRC)
TC   = TransactChat


$(TC):
	@mkdir -p bin
	@$(HC) $(ARGS) -main-is $(TC).main $(SRC)$(TC).hs -o $(BIN)$(TC)

clean:
	@rm -rf $(BIN)
	@rm $(SRC)*.o
	@rm $(SRC)*.hi

run: $(TC)
	@$(BIN)$(TC)
