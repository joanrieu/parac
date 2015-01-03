ANTLR=/usr/share/java/antlr-complete.jar

bin/: src/ParaC.c $(wildcard src/**.java)
	@echo 'Generating the parser...'
	env CLASSPATH=$(ANTLR) antlr4 -o bin/ -package net.fififox.ParaC src/ParaC.g4
	@echo 'Compiling the compiler...'
	env CLASSPATH=bin/:$(ANTLR) find . -name "*.java" -exec javac -verbose -d bin/ {} +

clean:
	rm -rf bin/

.PHONY: clean
