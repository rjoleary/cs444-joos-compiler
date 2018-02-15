# Can be Jlr1 or Jlalr1
GRAMMAR  := Jlalr1

HS_LIB   := src/lib/haskell
HS_LIB_FILES := $(sort $(wildcard ${HS_LIB}/*.hs))

LEXER_SRC  := src/haskell
LEXER_FILES := $(sort $(wildcard ${LEXER_SRC}/*.hs))
LEXER_MAIN := ${LEXER_SRC}/Lexer.hs

HS_TEST := test/haskell

GHC = ghc -O2 -Wall -i"${HS_LIB}:${LEXER_SRC}:${HS_TEST}"


.PHONY : compiler all zip clean report grammar test.positive test.negative test test.unit

# Only builds the compiler. This is the recipe run by Marmoset.
compiler : bin bin/parser bin/lexer bin/weeder bin/ast

# Builds everything including the grammar and report.
all : compiler grammar report

zip :
	rm -f submission.zip
	zip submission `git ls-files`

bin :
	mkdir -p bin

bin/lexer : bin ${LEXER_FILES} ${HS_LIB_FILES}
	${GHC} ${LEXER_MAIN} -o bin/lexer

bin/parser : bin src/rust/parser.rs
	rustc --codegen opt-level=2 src/rust/parser.rs -o bin/parser

bin/weeder : bin src/weeder/weeder.hs ${HS_LIB_FILES}
	${GHC} -o bin/weeder src/weeder/weeder.hs

bin/ast : $(wildcard src/ast/*.hs)
	${GHC} -o bin/ast $^

report : report.pdf

report.pdf : README.md
	pandoc -V geometry:margin=1in -o $@ $<

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

test.unit :
	stack exec runghc -- -i"${HS_SRC}:${HS_TEST}" test/haskell/UnitTest.hs

test.positive : compiler
	@./testrunner.sh positive

test.negative : compiler
	@./testrunner.sh negative

test.java : compiler
	@COMPILER=javac ./testrunner.sh positive

test : compiler
	@./testrunner.sh all

clean :
	rm -rf bin/ report.pdf src/java/jlalr/*.class src/haskell/*.o src/haskell/*.hi submission.zip
