TESTNAME=`basename $0`

EXPECTED="Error: open_in: Cannot open stream from \`foo'."
# foo does not exist 
QUERY="open_in \"foo\" X."
gives_error_toplevel "$QUERY" "$EXPECTED" "$TESTNAME"
