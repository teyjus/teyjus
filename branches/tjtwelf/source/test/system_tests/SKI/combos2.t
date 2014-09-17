use lib '../lib';
use strict;
use Test::More tests => 5;

my $TJSIM = "../../tjsim";
my $PATH = "--path SKI/";
my $MODULE = "combos2";
my $code;
my $ans;


############################################
############################################
$code = <<'CODE';
single_test \"(I K)\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = abs (W1\ abs (W2\ W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"single_test");

############################################
############################################
$code = <<'CODE';
single_test \"((((I K) I) S) I)\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = abs (W1\ W1)


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"single_test");

############################################
############################################
$code = <<'CODE';
single_test \"((K (S S)) (I ((K (K S)) S)))\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = abs (W1\ abs (W2\ abs (W3\ app (app W2 W3) (app (app W1 W2) W3))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"single_test");

############################################
############################################
$code = <<'CODE';
single_test \"((K S) S)\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = abs (W1\ abs (W2\ abs (W3\ app (app W1 W3) (app W2 W3))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"single_test");

############################################
############################################

$code = <<'CODE';
single_test \"((I (I K)) S)\" T.


CODE
$ans = <<'ANS';

The answer substitution:
T = abs (W1\ abs (W2\ abs (W3\ abs (W4\ app (app W2 W4) (app W3 W4)))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"single_test");
############################################
############################################
