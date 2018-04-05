#! /usr/bin/env bash

OUTPUT="$1"
INPUT="$2"

convert () {
    pandoc -f commonmark -V geometry:margin=1in
}

convert -o "$OUTPUT" "$INPUT"
