TESTNAME=`basename $0`
INPUT="3."
QUERY="read X, Y is X + 1." 
EXPECTED="Y = 4
X = 3"
gives_result_stdin "$INPUT" "$QUERY" "$EXPECTED"  "$TESTNAME" 
