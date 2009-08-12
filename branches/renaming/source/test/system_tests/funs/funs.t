use lib '../lib';
use strict;
use Test::More tests => 7;

my $TJSIM = "../../tjsim";
my $PATH = "-p funs/";
my $MODULE = "funs";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
mapfun (X\ (g a X)) [a,b] L.
CODE
$ans = <<'ANS';

The answer substitution:
L = g a a :: g a b :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");

############################################
############################################
$code = <<'CODE';
mapfun F [a] [g a a].
CODE
$ans = <<'ANS';

The answer substitution:
F = F

The remaining disagreement pairs list:
<F a, g a a>
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");

############################################
############################################
$code = <<'CODE';
mapfun F [a,b] [g a a, g a b].
CODE
$ans = <<'ANS';

The answer substitution:
F = F

The remaining disagreement pairs list:
<F b, g a b>
<F a, g a a>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");

############################################
############################################
$code = <<'CODE';
mapfun F [X] [g a a].
CODE
$ans = <<'ANS';

The answer substitution:
X = X
F = F

The remaining disagreement pairs list:
<F X, g a a>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");

############################################
############################################

$code = <<'CODE';
foldr F 5 [1,2,3,4] L.
CODE
$ans = <<'ANS';

The answer substitution:
L = F 1 (F 2 (F 3 (F 4 5)))
F = F

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"foldr");

############################################
############################################
$code = <<'CODE';
foldl F 5 [1,2,3,4] L.
CODE
$ans = <<'ANS';

The answer substitution:
L = F (F (F (F 5 4) 3) 2) 1
F = F

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"foldl");

############################################
############################################
$code = <<'CODE';
foldl F 5 [1,2,3,4] (5 + 4 + 3 + 2 + 1).
CODE
$ans = <<'ANS';

The answer substitution:
F = F

The remaining disagreement pairs list:
<F (F (F (F 5 4) 3) 2) 1, 5 + 4 + 3 + 2 + 1>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"foldl");
############################################
############################################
