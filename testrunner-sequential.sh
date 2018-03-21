#!/bin/bash

COMPILER=${COMPILER-timeout 5s ./stdjoosc}
TESTSET=${TESTSET-test}

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

    local PROCESSES=""

    # Execute all the tests in parallel.
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

        $COMPILER $files > "$testname.stdout" 2> "$testname.stderr"; ret=$?

        RUN=$((RUN+1))
        case $ret in
            $PASS_CODE)
                PASSED=$((PASSED+1))
                echo -n PASSED
                ;;
            $FAIL_CODE)
                FAILED=$((FAILED+1))
                echo -n FAILED "($FAIL_CODE)"
                ;;
            *)
                ERROR=$((ERROR+1))
                echo -n ERROR
                ;;
        esac

        echo '' $testname

        cat "$testname.stderr" | sed 's/^/  /'
    done
}

# Delete intermediate files from previous testing.
find 'test' -name '*.ast' -delete
find 'test' -name '*.tokens' -delete
find 'test' -name '*.parse' -delete
find 'test' -name '*.stdout' -delete
find 'test' -name '*.stderr' -delete

POSITIVE_TESTS=$(echo $TESTSET/positive/*)
NEGATIVE_TESTS=$(echo $TESTSET/negative/*)

echo 'Building stdlib...'
runtests 0 42 'test/positive/Empty.java'
export SKIP_STDLIB=1
echo 'Running tests...'

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
echo "  Passed: $PASSED/$RUN = $(echo "scale=3; $PASSED/$RUN*100" | bc)%"
echo "  Failed: $FAILED/$RUN = $(echo "scale=3; $FAILED/$RUN*100" | bc)%"
echo "  Error: $ERROR/$RUN = $(echo "scale=3; $ERROR/$RUN*100" | bc)%"
echo "  Ignored: $IGNORED"
