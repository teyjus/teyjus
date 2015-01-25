SH=bash
export TJSIM=../../../tjsim


# Allow to test inputs given from stdin
function gives_result_stdin {
	# $1 : input from stdin
	# $2 : query 
	# $3 : expected result
	# $4 : name of the test 
	RES=`echo "$1" | "$TJSIM" --solve "$2" | sed -n '5 p'`
	if [ "$RES" = "$3" ]; then
		echo "$4" "success"
		exit 0
	else
		RES=`echo "$1" | "$TJSIM" --solve "$2"`
		echo "$4 failure (the result $3 was expected but got $RES"
		echo "" 
		exit -1
	fi
}

function gives_result {
	# $1 : query 
	# $2 : expected result
	# $3 : name of the test 
	RES=`"$TJSIM" --batch --solve "$1" | sed -n '3 p'`
	if [ "$RES" = "$2" ]; then
		echo "$3" "success"
		exit 0
	else
		RES=`"$TJSIM" --batch --solve "$1"`
		echo "$3 failure (the result $2 was expected but got: $RES" 
		echo ""
		exit -1
	fi
}


function gives_error_file {
	# $1 : query 
	# $2 : expected error
	# $3 : name of the test 
	# $4 : name of the test 
	RES=`"$TJSIM" "$4" --batch --solve "$1" 2>&1 > /dev/null`
	if [ "$RES" = "$2" ]; then
		echo "$3" "success"
		exit 0
	else
		echo "$3 failure (the error $2 was expected but got: $RES" 
		echo ""
		exit -1
	fi
}

function gives_error_toplevel {
	# $1 : query 
	# $2 : expected error
	# $3 : name of the test 
	RES=`"$TJSIM" --batch --solve "$1" 2>&1 > /dev/null`
	if [ "$RES" = "$2" ]; then
		echo "$3" "success"
		exit 0
	else
		echo "$3 failure (the error $2 was expected but got: $RES" 
		echo ""
		exit -1
	fi
}

export -f gives_result_stdin
export -f gives_result
export -f gives_error_file
export -f gives_error_toplevel


# We want to run all tests
for dir in $(ls); do
	if [ -d $dir ]; then
		cd $dir
		for test in $(ls *.sh); do
			$SH $test	
		done
		cd ..
	fi
done
