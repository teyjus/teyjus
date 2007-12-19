use lib '../lib';
use strict;
use Test::More tests => 12;

my $TJSIM = "../../tjsim";
my $PATH = "--path handbook_progs_eval/";
my $MODULE = "eval_examples";
my $code;
my $ans;

############################################
############################################

$code = <<'CODE';
eval (app (abs f\ (abs x\ (abs y\ (app (app f x) y)))) (abs u\ (abs v\ u))) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = abs (W1\ abs (W2\ app (app (abs (W3\ abs (W4\ W3))) W1) W2))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################
$code = <<'CODE';
eval (app (abs f\ (app (app f (abs x\ x)) (abs x\ (abs y\ y)))) (abs u\ (abs v\ u))) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = abs (W1\ W1)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");


############################################
############################################

$code = <<'CODE';
eval (app (abs f\ (app (app f (abs x\ x)) (abs x\ (abs y\ y)))) (abs u\ (abs v\ v))) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = abs (W1\ abs (W2\ W2))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################

$code = <<'CODE';
eval (eq (c 5) (c 0)) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = false

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################

$code = <<'CODE';
eval (eq (c 5) (c 5)) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = truth

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################

$code = <<'CODE';
eval (lss (c 5) (c 3)) R.
CODE
$ans = <<'ANS';


The answer substitution:
R = false

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################

$code = <<'CODE';
eval (lss (c 3) (c 5)) R.
CODE
$ans = <<'ANS';

The answer substitution:
R = truth

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");

############################################
############################################

$code = <<'CODE';
test 1 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = c 120

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");
############################################
############################################

$code = <<'CODE';
test 2 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = c 1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");
############################################
############################################

$code = <<'CODE';
test 3 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = c 5

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");
############################################
############################################

$code = <<'CODE';
test 4 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = c 3

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");
############################################
############################################

$code = <<'CODE';
test 5 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = cons (c 1) (cons (c 2) (cons (c 3) (cons (c 4) null)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"eval");
############################################
############################################
