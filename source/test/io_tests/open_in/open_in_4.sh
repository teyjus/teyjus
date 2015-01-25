TESTNAME=`basename $0`

EXPECTED="Error: open_in: Expected filename, found unbound variable."
QUERY="open_in Unbound X."
gives_error_toplevel "$QUERY" "$EXPECTED" "$TESTNAME"
