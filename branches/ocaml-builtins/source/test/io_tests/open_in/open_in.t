use lib '../lib';
use strict;
use Test::More tests => 4;

my $TJSIM = "../../tjsim";
my $PATH = "-p open_in/";
my $MODULE = "open_in";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
test3 X.
CODE
$ans = <<'ANS';

The answer substitution:
X = <stream -- "open_in/bar">

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"open_in");

############################################
# In the following test: we should test that we have the following on stderr:
#Error: open_in: Expected filename, found unbound variable.
############################################
$code = <<'CODE';
test4 X.
CODE
$ans = <<'ANS';

The answer substitution:
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"open_in");

