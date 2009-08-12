use lib '../lib';
use strict;
use Test::More tests => 6;

my $TJSIM = "../../tjsim";
my $PATH = "-p append/";
my $MODULE = "append";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
append (2::nil) (3::nil) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = 2 :: 3 :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"append");
############################################
############################################
$code = <<'CODE';
append L1 L2 (1::2::3::4::5::nil).
CODE
$ans = <<'ANS';

The answer substitution:
L2 = 1 :: 2 :: 3 :: 4 :: 5 :: nil
L1 = nil


The answer substitution:
L2 = 2 :: 3 :: 4 :: 5 :: nil
L1 = 1 :: nil


The answer substitution:
L2 = 3 :: 4 :: 5 :: nil
L1 = 1 :: 2 :: nil


The answer substitution:
L2 = 4 :: 5 :: nil
L1 = 1 :: 2 :: 3 :: nil


The answer substitution:
L2 = 5 :: nil
L1 = 1 :: 2 :: 3 :: 4 :: nil


The answer substitution:
L2 = nil
L1 = 1 :: 2 :: 3 :: 4 :: 5 :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"append");

############################################
############################################
$code = <<'CODE';
pi (X \ (append (X::nil) (2::X::nil) (F X))).
CODE
$ans = <<'ANS';

The answer substitution:
F = W1\ W1 :: 2 :: W1 :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"pi append");

############################################
############################################
$code = <<'CODE';
append (F 2) (3::nil) (2::2::3::nil).
CODE
$ans = <<'ANS';

The answer substitution:
F = F

The remaining disagreement pairs list:
<F 2, 2 :: 2 :: nil>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"append");

############################################
############################################
$code = <<'CODE';
append (F X) nil (2::3::2::5::nil).
CODE
$ans = <<'ANS';

The answer substitution:
X = X
F = F

The remaining disagreement pairs list:
<F X, 2 :: 3 :: 2 :: 5 :: nil>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"append");

############################################
############################################
$code = <<'CODE';
sigma X \ (append [2] [3,3] (F X)).
CODE
$ans = <<'ANS';

The answer substitution:
F = F

The remaining disagreement pairs list:
<F _T1, 2 :: 3 :: 3 :: nil>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"append");
