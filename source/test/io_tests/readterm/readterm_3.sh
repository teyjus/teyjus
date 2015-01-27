TESTNAME=`basename $0`
QUERY="open_in \"bar\" Stream, readterm Stream X." 
EXPECTED="X = g (g (g f))
Stream = <stream -- \"bar\">"
MODFILE="readterm"
gives_result_file "$QUERY" "$EXPECTED"  "$TESTNAME"  "$MODFILE"
