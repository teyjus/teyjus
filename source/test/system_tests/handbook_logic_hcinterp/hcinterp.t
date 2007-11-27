use lib '../lib';
use strict;
use Test::More tests => 1;

my $TJSIM = "../../tjsim";
my $PATH = "--path handbook_logic_hcinterp/";
my $MODULE = "hcinterp_examples";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
pathfroma X.
CODE
$ans = <<'ANS';

The answer substitution:
X = b


The answer substitution:
X = c


The answer substitution:
X = f c

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"pathfroma");
############################################
############################################