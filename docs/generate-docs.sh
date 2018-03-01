#! /usr/bin/env bash

convert () {
    pandoc -f commonmark -V geometry:margin=1in $@
}

convert $@
