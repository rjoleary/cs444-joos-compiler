RS_FILES := $(wildcard src/*.rs)

.PHONY : all clean

joosc : ${RS_FILES}
	cargo build --release
	ln -sf target/release/joosc .

all : joosc docs

docs : docs.pdf

docs.pdf : docs.md
	pandoc -V geometry:margin=1in -o $@ $<

clean :
	rm -rf docs.pdf joosc target/
