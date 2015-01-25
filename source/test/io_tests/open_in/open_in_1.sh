TESTNAME=`basename $0`

EXPECTED="X = <stream -- \"bar\">"
QUERY="open_in \"bar\" X."
gives_result "$QUERY" "$EXPECTED" "$TESTNAME"
