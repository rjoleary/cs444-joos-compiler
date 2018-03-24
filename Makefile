# Can be Jlr1 or Jlalr1
GRAMMAR  := Jlalr1

GHC_WARNINGS := -w -Wmissing-fields

GHC = stack build --only-dependencies && stack ghc -- -outputdir ${HS_BUILD} -O2 ${GHC_WARNINGS} -i${HS_INCLUDE}

DOC_INPUT := $(sort $(shell find ./docs -name '*.md'))
DOC_OUTPUT := $(patsubst %.md,%.pdf,${DOC_INPUT})

HS_LIB   := src/lib
HS_LIB_FILES := $(shell find ${HS_LIB} -name '*.hs')

COMPILER_SRC  := src/compiler
COMPILER_FILES := $(shell find ${COMPILER_SRC} -name '*.hs')
COMPILER_MAIN := ${COMPILER_SRC}/Main.hs

LEXER_SRC  := src/lexer
LEXER_FILES := $(shell find ${LEXER_SRC} -name '*.hs')
LEXER_MAIN := ${LEXER_SRC}/Lexer.hs

WEEDER_SRC  := src/weeder
WEEDER_FILES := $(shell find ${WEEDER_SRC} -name '*.hs')

HS_BUILD := ghc_build

HS_TEST := test/haskell

HS_INCLUDE := "${HS_LIB}:${LEXER_SRC}:${COMPILER_SRC}:${HS_TEST}"

ALL_HS_FILES := ${HS_LIB_FILES} ${LEXER_FILES} ${WEEDER_FILES} ${COMPILER_FILES}

# Includes all the files tracked by git.
ZIP_FILES := $(shell git ls-files)

.PHONY : compiler all zip clean docs grammar test.positive test.negative \
	test test.unit hfmt ghci

# Only builds the compiler. This is the recipe run by Marmoset.
compiler : bin bin/parser bin/lexer bin/weeder bin/compiler

# Builds everything including the grammar and report.
all : compiler grammar docs

zip : joosc
	rm -f submission.zip
	@zip submission $(ZIP_FILES) --exclude 'test/*'

bin :
	mkdir -p bin

bin/lexer : ${LEXER_FILES} ${HS_LIB_FILES}
	${GHC} ${LEXER_MAIN} -o bin/lexer

bin/parser : src/rust/parser.rs
	rustc --codegen opt-level=2 src/rust/parser.rs -o bin/parser

bin/weeder : src/weeder/weeder.hs ${HS_LIB_FILES}
	${GHC} -o bin/weeder src/weeder/weeder.hs

bin/compiler : ${COMPILER_FILES} ${HS_LIB_FILES}
	${GHC} -o $@ $^

docs : ${DOC_OUTPUT}

docs/%.pdf : docs/%.md
	./docs/generate-docs.sh $@ $<

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

# Format haskell code
hfmt :
	stack build hfmt && stack exec hfmt -- -w src

ghci :
	stack ghci --ghci-options -i"${HS_INCLUDE}"

test.unit :
	stack exec runghc -- -i"${HS_INCLUDE}" test/haskell/UnitTest.hs

test.positive : compiler
	@./testrunner.sh positive

test.negative : compiler
	@./testrunner.sh negative

test.java : compiler
	@COMPILER=javac ./testrunner.sh positive

test : compiler
	@./testrunner.sh all

test.a1 : compiler
	@TESTNUM=1 TESTSET=test/marmoset/a1 ./testrunner.sh all

test.a2 : compiler
	@TESTNUM=2 TESTSET=test/marmoset/a2 ./testrunner.sh all

test.a3 : compiler
	@TESTNUM=3 TESTSET=test/marmoset/a3 ./testrunner.sh all

test.a4 : compiler
	@TESTNUM=4 TESTSET=test/marmoset/a4 ./testrunner.sh all

test.a5 : compiler
	@./testrunner-codegen.sh

test.all : compiler
	@OUT="A1 $$(make test.a1 | tee /dev/stderr | grep Passed)\n"; \
	OUT="$${OUT}A2 $$(make test.a2 | tee /dev/stderr | grep Passed)\n"; \
	OUT="$${OUT}A3 $$(make test.a3 | tee /dev/stderr | grep Passed)\n"; \
	OUT="$${OUT}A4 $$(make test.a4 | tee /dev/stderr | grep Passed)\n"; \
	OUT="$${OUT}A5 $$(make test.a5 | tee /dev/stderr | grep Passed)\n"; \
	echo -e "\n\n$${OUT}"

clean :
	find src \( -name '*.o' -o -name '*.hi' \) -delete
	find test \( -name '*.tokens' -o -name '*.parse' -o -name '*.ast' \) -delete
	rm -rf bin/ ${DOC_OUTPUT} src/java/jlalr/*.class submission.zip ${HS_BUILD}
