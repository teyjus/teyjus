use lib '../lib';
use strict;
use Test::More tests =>  11;

my $TJSIM = "../../tjsim";
my $PATH = "-p maps/";
my $MODULE = "maps";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
mapfun [a,b] (X\ (g a X)) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = g a a :: g a b :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");

############################################
############################################
$code = <<'CODE';
mapfun [a] F [g a a].
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
mapfun [a,b] F [g a a, g a b].
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
mapfun [X] F [g a a].
CODE
$ans = <<'ANS';

The answer substitution:
F = F
X = X

The remaining disagreement pairs list:
<F X, g a a>


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mapfun");


############################################
############################################

$code = <<'CODE';
mappred [jane,john,james] father L.
CODE
$ans = <<'ANS';

The answer substitution:
L = moses :: peter :: peter :: nil


The answer substitution:
L = john :: peter :: peter :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mappred");

############################################
############################################

$code = <<'CODE';
mappred [jane,james] (X1\ X2\ (sigma Y\ ((father X1 Y), (father X2 Y)))) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = jane :: john :: nil


The answer substitution:
L = jane :: james :: nil


The answer substitution:
L = jane :: john :: nil


The answer substitution:
L = jane :: james :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mappred");

############################################
############################################
#skipped#
$code = <<'CODE';
mappred [jane,james] P L.

CODE
$ans = <<'ANS';
Error: solve: Ill-formed goal: uninstantiated variable as head.
ANS
# Note the 2>&1 which redirects STDERR to STDOUT
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE 2>&1\n`, $ans,"mappred");

############################################
############################################

$code = <<'CODE';
mappred [john,peter] (X\ Y\ ((father Y X) ; (father X Y))) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = jane :: john :: nil


The answer substitution:
L = jane :: james :: nil


The answer substitution:
L = jane :: charles :: nil


The answer substitution:
L = peter :: john :: nil


The answer substitution:
L = peter :: james :: nil


The answer substitution:
L = peter :: charles :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mappred");

############################################
############################################

$code = <<'CODE';
mappred [john] (X\ Y\ ((father Y X) ; (father Y X))) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = jane :: nil


The answer substitution:
L = jane :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mappred");

############################################
############################################

$code = <<'CODE';
reduce [1,2,3,4,5] (A\ B\ (A + B)) 0 X.
CODE
$ans = <<'ANS';

The answer substitution:
X = 1 + (2 + (3 + (4 + (5 + 0))))


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"reduce");

############################################
############################################
$code = <<'CODE';
reduce_eval [1,2,3] (X\ Y\ (X + Y)) 0 Z.
CODE
$ans = <<'ANS';

The answer substitution:
Z = 6

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"reduce_eval");
############################################
############################################
