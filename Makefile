# Can be Jlr1 or Jlalr1
GRAMMAR  := Jlalr1

TESTS_POSITIVE := $(wildcard test/positive/*.joos)
TESTS_NEGATIVE := $(wildcard test/negative/*.joos)

GHC = ghc -Wall

HS_FILES := $(wildcard src/haskell/*.hs)

.PHONY : compiler all zip clean docs grammar test.positive

# Only builds the compiler. This is the recipe run by Marmoset.
compiler : bin bin/parser bin/lexer bin/weeder

# Builds everything including the grammar and docs.
all : compiler grammar docs

zip :
	rm -f submission.zip
	zip submission `git ls-files`

bin :
	mkdir -p bin

bin/lexer : bin ${HS_FILES}
	${GHC} -o bin/lexer ${HS_FILES}

bin/parser : bin src/rust/parser.rs
	rustc src/rust/parser.rs -o bin/parser

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

test.positive :
	@run=0; \
	passed=0; \
	failed=0; \
	error=0; \
	for file in ${TESTS_POSITIVE} ; do \
		run=$$((run+1)) ; \
		echo -n "$$run: $$file... "; \
		rm -f test/joos_{input,tokens,tree}.txt; \
		cp "$$file" "test/joos_input.txt"; \
		./joosc > /dev/null 2> /dev/null; \
		case $$? in \
			0) passed=$$((passed+1)); echo PASSED ;; \
			42) failed=$$((failed+1)); echo FAILED ;; \
			*) error=$$((error+1)); echo ERROR ;; \
		esac; \
	done; \
	echo; \
	echo "SUMMARY:"; \
	echo "  Passed: $$passed/$$run"; \
	echo "  Failed: $$failed/$$run"; \
	echo "  Error: $$error/$$run"

clean :
	rm -rf bin/ docs.pdf src/java/jlalr/*.class src/haskell/*.o src/haskell/*.hi submission.zip
