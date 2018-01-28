# TODO: change to joos at some point
GRAMMAR := appel

RS_FILES := $(wildcard src/*.rs)

.PHONY : all clean docs grammar

joosc : ${RS_FILES}
	cargo build --release
	ln -sf target/release/joosc .

all : joosc grammar docs

docs : docs.pdf

grammar : def/${GRAMMAR}.lr1

src/java/jlalr/Jlr1.class : src/java/jlalr/Jlalr1.java
	javac src/java/jlalr/Jlalr1.java

def/${GRAMMAR}.lr1 : src/java/jlalr/Jlr1.class def/${GRAMMAR}.cfg
	java -classpath src/java jlalr.Jlr1 < def/${GRAMMAR}.cfg > def/${GRAMMAR}.lr1

docs.pdf : docs.md
	pandoc -V geometry:margin=1in -o $@ $<

clean :
	rm -rf docs.pdf joosc target/ src/java/jlalr/*.class
