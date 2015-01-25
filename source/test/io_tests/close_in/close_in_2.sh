TESTNAME=`basename $0`

EXPECTED="Error: close_in: Attempting to close an input stream that is already closed."
QUERY="open_in \"bar\" X, close_in X, close_in X."
gives_error_toplevel "$QUERY" "$EXPECTED" "$TESTNAME"
