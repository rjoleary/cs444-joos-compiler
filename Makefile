# TODO: change to joos at some point
GRAMMAR := appel

# TODO: move rust source to src/rust/*.rs
RS_FILES := $(wildcard src/*.rs)
HS_FILES := $(wildcard src/haskell/*.hs)

.PHONY : compiler all clean docs grammar

# Only builds the compiler. This is the recipe run by Marmoset.
compiler : bin bin/parser bin/haskell_main

# Builds everything including the grammar and docs.
all : compiler grammar docs

bin :
	mkdir -p bin

bin/parser : ${RS_FILES}
	cargo build --release
	cp target/release/parser bin/parser

bin/haskell_main : ${HS_FILES}
	ghc -o bin/haskell_main ${HS_FILES}

docs : docs.pdf

grammar : def/${GRAMMAR}.lr1

src/java/jlalr/Jlr1.class : src/java/jlalr/Jlalr1.java
	javac src/java/jlalr/Jlalr1.java

def/${GRAMMAR}.lr1 : src/java/jlalr/Jlr1.class def/${GRAMMAR}.cfg
	java -classpath src/java jlalr.Jlr1 < def/${GRAMMAR}.cfg > def/${GRAMMAR}.lr1

docs.pdf : docs.md
	pandoc -V geometry:margin=1in -o $@ $<

clean :
	rm -rf bin/ docs.pdf target/ src/java/jlalr/*.class src/haskell/*.o src/haskell/*.hi
