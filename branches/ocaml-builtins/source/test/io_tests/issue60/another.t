use lib '../lib';
use strict;
use Test::More tests => 1;

my $TJSIM = "../../tjsim";
my $PATH = "-p issue60/";
my $MODULE = "another";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
test X.
CODE
$ans = <<'ANS';

The answer substitution:

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"another");
