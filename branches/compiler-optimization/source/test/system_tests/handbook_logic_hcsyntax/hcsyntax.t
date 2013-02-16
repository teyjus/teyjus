use lib '../lib';
use strict;
use Test::More tests => 8;

my $TJSIM = "../../tjsim";
my $PATH = "--path handbook_logic_hcsyntax/";
my $MODULE = "hcsyntax_examples";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
test_goal 1 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = some (W1\ path a W1 and path W1 b)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_goal 2 F.
CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_goal 3 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = path a b and path b a

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_goal 4 F.
CODE
$ans = <<'ANS';

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_defcl 4 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = path a b imp path b a

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_defcl 5 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = path a b imp adj a b

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_defcl 6 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = all (W1\ all (W2\ path W1 W2 imp adj W1 W2))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
$code = <<'CODE';
test_defcl 7 F.
CODE
$ans = <<'ANS';

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");

############################################
############################################
