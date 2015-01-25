TESTNAME=`basename $0`

EXPECTED="Error: close_in: Expected stream, found unbound variable."
QUERY="close_in Unbound."
gives_error_toplevel "$QUERY" "$EXPECTED" "$TESTNAME"
