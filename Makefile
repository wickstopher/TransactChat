# Makefile for TransactChat

HC   = ghc
SRC  = src/
BIN  = bin/
ODIR = .output/
ARGS = -i$(SRC) -outputdir $(ODIR)
TC   = TransactChat


$(TC): setup
	@$(HC) $(ARGS) -main-is $(TC).main $(SRC)$(TC).hs -o $(BIN)$(TC)

clean:
	@rm -rf $(BIN)
	@rm -rf $(ODIR)

run: $(TC)
	@$(BIN)$(TC)

setup:
	@mkdir -p $(BIN)
	@mkdir -p $(ODIR)