use lib '../lib';
use strict;
use Test::More tests => 5;

my $TJSIM = "../../tjsim";
my $PATH = "-p pcf_eval/";
my $MODULE = "eval_test";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
eval_test 1 V.

CODE
$ans = <<'ANS';

The answer substitution:
V = in 144

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval_test");

############################################
############################################
$code = <<'CODE';
eval_test 2 V.

CODE
$ans = <<'ANS';

The answer substitution:
V = cons @ in 2 @ (cons @ in 8 @ empty)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval_test");

############################################
############################################
$code = <<'CODE';
eval_test 3 V.

CODE
$ans = <<'ANS';

The answer substitution:
V = cons @ in 3 @ (cons @ in 5 @ empty)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval_test");

############################################
############################################
$code = <<'CODE';
eval_test 4 V.

CODE
$ans = <<'ANS';

The answer substitution:
V = truth

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval_test");

############################################
############################################
$code = <<'CODE';
eval_test 5 V.

CODE
$ans = <<'ANS';

The answer substitution:
V = false

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval_test");
############################################
############################################