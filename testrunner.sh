#!/bin/bash

COMPILER=${COMPILER-./joosc}

RUN=0
PASSED=0
FAILED=0
ERROR=0
IGNORED=0

# runtests <PASS_CODE> <FAIL_CODE> <TESTS>...
runtests() {
    local PASS_CODE=$1
    local FAIL_CODE=$2
    shift 2

    for testname in "$@" ; do
        if echo "$testname" | grep IGNORE > /dev/null; then
            echo "IGNORED $testname"
            IGNORED=$((IGNORED+1))
            continue
        fi

        # If testname is a directory, run all the tests inside it.
        if [ -d $testname ]; then
            local files=$(find $testname -name '*.java')
        else
            local files=$testname
        fi

        RUN=$((RUN+1))
        echo -n "$RUN: $testname... "
        rm -f test/joos_{input,tokens,tree}.txt
        $COMPILER $files > /dev/null
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


# Delete intermediate files from previous testing.
find 'test' -name '*.tokens' -delete
find 'test' -name '*.parse' -delete

POSITIVE_TESTS=$(echo test/positive/*)
NEGATIVE_TESTS=$(echo test/negative/*)

case "$1" in
    positive)
        runtests 0 42 $POSITIVE_TESTS
        ;;
    negative)
        runtests 42 0 $NEGATIVE_TESTS
        ;;
    all)
        runtests 0 42 $POSITIVE_TESTS
        runtests 42 0 $NEGATIVE_TESTS
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
