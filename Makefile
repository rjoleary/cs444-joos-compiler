.PHONY : all clean

joosc :

all : joosc docs

docs : docs.pdf

docs.pdf : docs.md
	pandoc -V geometry:margin=1in -o $@ $<

clean :
	rm -f docs.pdf
