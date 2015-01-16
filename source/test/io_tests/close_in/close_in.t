use lib '../lib';
use strict;
use Test::More tests => 3;

my $TJSIM = "../../tjsim";
my $PATH = "-p close_in/";
my $MODULE = "close_in";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
test1 X.
CODE
$ans = <<'ANS';

The answer substitution:
X = <stream -- "open_in/bar">

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"open_in");

############################################
# In the following test: we should test that we have the following on stderr:
#Error: close_in: Attempting to close an input stream that is already closed.
############################################
$code = <<'CODE';
test2 X.
CODE
$ans = <<'ANS';

The answer substitution:
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"open_in");

############################################
# In the following test: we should test that we have the following on stderr:
#Error: close_in: Expected stream, found unbound variable.
############################################
$code = <<'CODE';
test3.
CODE
$ans = <<'ANS';

The answer substitution:
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"open_in");

