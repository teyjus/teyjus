# Notice that this test for issue 60 is not the same as when we test for 
# closing two times the same file in the close_in test folder since
# the issue happened when the closing instruction was in file, not at toplevel.

TESTNAME=`basename $0`
QUERY="test."
FILE="another"
EXPECTED="Error: close_in: Attempting to close an input stream that is already closed."
gives_error_file "$QUERY" "$EXPECTED" "$TESTNAME" "$FILE"
