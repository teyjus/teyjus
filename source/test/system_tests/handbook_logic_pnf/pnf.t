use lib '../lib';
use strict;
use Test::More tests => 4;

my $TJSIM = "../../tjsim";
my $PATH = "--path handbook_logic_pnf/";
my $MODULE = "pnf_examples";
my $code;
my $ans;

###################################################
###################################################
$code = <<'CODE';
test 1 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = some (W1\ path a W1 imp tru)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

###################################################
###################################################
$code = <<'CODE';
test 2 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = all (W1\ path a W1 imp tru)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

###################################################
###################################################
$code = <<'CODE';
test 3 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = all (W1\ path a W1 and path W1 a)


The answer substitution:
F = all (W1\ all (W2\ path a W1 and path W2 a))


The answer substitution:
F = all (W1\ all (W2\ path a W2 and path W1 a))
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

###################################################
###################################################
$code = <<'CODE';
test 4 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = all (W1\ all (W2\ path a W1 imp path a W2))


The answer substitution:
F = all (W1\ all (W2\ path a W2 imp path a W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
##################################################
##################################################