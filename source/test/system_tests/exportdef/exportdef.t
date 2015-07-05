use lib '../lib';
use strict;
use Test::More skip_all => "Known bug to fix";

my $TJSIM = "../../tjsim";
my $PATH = "-p exportdef/";
my $MODULE = "main";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
q X.
CODE
$ans = <<'ANS';

The answer substitution:
X = 1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"exportdef");
