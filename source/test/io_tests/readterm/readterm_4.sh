TESTNAME=`basename $0`
QUERY="open_in \"bar_multiple\" Stream, readterm Stream X." 
EXPECTED="X = g (g (g f))
Stream = <stream -- \"bar_multiple\">"
MODFILE="readterm"
gives_result_file "$QUERY" "$EXPECTED"  "$TESTNAME"  "$MODFILE"
