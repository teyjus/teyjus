use lib '../lib';
use strict;
use Test::More tests => 2;

my $TJSIM = "../../tjsim";
my $PATH = "-p reverse/";
my $MODULE = "reverse";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
reverse (1::2::3::4::nil) L.
CODE
$ans = <<'ANS';

The answer substitution:
L = 4 :: 3 :: 2 :: 1 :: nil

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"reverse");

############################################
############################################
$code = <<'CODE';
reverse (b::a::nil) (F X).
CODE
$ans = <<'ANS';

The answer substitution:
X = X
F = F

The remaining disagreement pairs list:
<F X, a :: b :: nil>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"reverse");
############################################
############################################