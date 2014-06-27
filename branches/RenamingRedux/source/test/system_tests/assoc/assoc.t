use lib '../lib';
use strict;
use Test::More tests => 3;

my $TJSIM = "../../tjsim";
my $PATH = "--path assoc/";
my $MODULE = "assoc";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
assoc (foo:i) (X:int) [pair bar 2, pair foo 1].
CODE
$ans = <<'ANS';

The answer substitution:
X = 1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"assoc");

############################################
############################################
$code = <<'CODE';
assoc (F:i) (X:int) [pair bar 2, pair foo 1].
CODE
$ans = <<'ANS';

The answer substitution:
X = 2
F = bar

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"assoc");

############################################
############################################
$code = <<'CODE';
assoc ((F:i -> i) Y) (X:int) [pair bar 2, pair foo 1].
CODE
$ans = <<'ANS';


The answer substitution:
X = 2
Y = Y
F = F

The remaining disagreement pairs list:
<F Y, bar>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"assoc");
############################################
############################################
