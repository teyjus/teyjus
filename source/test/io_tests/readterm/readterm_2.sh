TESTNAME=`basename $0`
QUERY="open_in \"foo\" Stream, readterm Stream X." 
EXPECTED="X = \"a string on a single line\"
Stream = <stream -- \"foo\">"
gives_result_stdin "$INPUT" "$QUERY" "$EXPECTED"  "$TESTNAME" 
