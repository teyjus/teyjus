TESTNAME=`basename $0`

EXPECTED="Error: open_in: The term to be bound is not a variable:  <stream -- \"bar\">"
# This fails since in the second call, X is already bound 
QUERY="open_in \"bar\" X, open_in \"bar\" X."
gives_error_toplevel "$QUERY" "$EXPECTED" "$TESTNAME"
