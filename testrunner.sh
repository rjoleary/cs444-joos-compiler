#!/bin/bash

RUN=0
PASSED=0
FAILED=0
ERROR=0
IGNORED=0

# runtests <PASS_CODE> <FAIL_CODE> <FILES>
runtests() {
    local PASS_CODE=$1
    local FAIL_CODE=$2
    shift 2

    for file in "$@" ; do
        if echo "$file" | grep IGNORE > /dev/null; then
            echo "IGNORED $file"
            IGNORED=$((IGNORED+1))
            continue
        fi

        RUN=$((RUN+1))
        echo -n "$RUN: $file... "
        rm -f test/joos_{input,tokens,tree}.txt
        ./joosc "$file" > /dev/null
        case $? in
            $PASS_CODE)
                PASSED=$((PASSED+1))
                echo PASSED
                ;;
            $FAIL_CODE)
                FAILED=$((FAILED+1))
                echo FAILED
                ;;
            *)
                ERROR=$((ERROR+1))
                echo ERROR
                ;;
        esac
    done
}


case "$1" in
    positive)
        runtests 0 42 $(ls test/positive/*.joos)
        ;;
    negative)
        runtests 42 0 $(ls test/negative/*.joos)
        ;;
    all)
        runtests 0 42 $(ls test/positive/*.joos)
        runtests 42 0 $(ls test/negative/*.joos)
        ;;
    *)
        echo "Error: Argument 0 must be positive, negative or all" >&2
        exit 2
        ;;
esac

echo
echo "SUMMARY:"
echo "  Passed: $PASSED/$RUN"
echo "  Failed: $FAILED/$RUN"
echo "  Error: $ERROR/$RUN"
echo "  Ignored: $IGNORED"
