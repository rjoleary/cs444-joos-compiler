# Can be Jlr1 or Jlalr1
GRAMMAR  := Jlalr1

GHC = ghc -Wall

HS_FILES := $(wildcard src/haskell/*.hs)

.PHONY : compiler all zip clean docs grammar

# Only builds the compiler. This is the recipe run by Marmoset.
compiler : bin bin/parser bin/haskell_main bin/weeder

# Builds everything including the grammar and docs.
all : compiler grammar docs

zip :
	rm -f submission.zip
	zip submission `git ls-files`

bin :
	mkdir -p bin

bin/lexer : bin src/haskell/lexer.hs
	${GHC} -o bin/lexer ${HS_FILES}

bin/parser : bin src/rust/parser.rs
	rustc src/rust/parser.rs -o bin/parser

bin/haskell_main : ${HS_FILES}
	${GHC} -o bin/haskell_main ${HS_FILES}

bin/weeder : src/weeder/weeder.hs
	ghc -o bin/weeder src/weeder/weeder.hs

docs : docs.pdf

grammar : def/joos.lr1

src/java/jlalr/${GRAMMAR}.class : src/java/jlalr/Jlalr1.java
	javac src/java/jlalr/Jlalr1.java

# cfg2 is a human-readable format of cfg.
bin/joos.cfg : bin def/joos.cfg2
	sed -e '/^#/d' -e '/^\s*$$/d' < def/joos.cfg2 > bin/productions.txt
	sed 's/\s\s*/\n/g' bin/productions.txt | sort -u > bin/symbols.txt
	cut -d' ' -f1 bin/productions.txt | sort -u > bin/nonterminals.txt
	comm -23 bin/symbols.txt bin/nonterminals.txt > bin/terminals.txt
	bash -c "cat <(wc -l < bin/terminals.txt) bin/terminals.txt  \
		<(wc -l < bin/nonterminals.txt) bin/nonterminals.txt     \
		<(echo S)                                                \
		<(wc -l < bin/productions.txt) bin/productions.txt" > bin/joos.cfg

def/joos.lr1 : src/java/jlalr/${GRAMMAR}.class bin/joos.cfg
	java -classpath src/java jlalr.${GRAMMAR} < bin/joos.cfg > def/joos.lr1

docs.pdf : docs.md
	pandoc -V geometry:margin=1in -o $@ $<

clean :
	rm -rf bin/ docs.pdf src/java/jlalr/*.class src/haskell/*.o src/haskell/*.hi submission.zip
