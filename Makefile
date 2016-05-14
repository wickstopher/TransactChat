# Makefile for TransactChat

TransactChat:
	@mkdir -p bin
	@ghc -main-is TransactChat.main src/TransactChat.hs  -o bin/TransactChat

clean:
	@rm bin/TransactChat
	@rm src/*.o
	@rm src/*.hi

run: TransactChat
	@bin/TransactChat

