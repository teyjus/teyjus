use lib '../lib';
use strict;
use Test::More tests => 9;

my $TJSIM = "../../tjsim";
my $PATH = "--path pcf_tr/";
my $MODULE = "tr_test";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
tr_test \"successor\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fn (W1\ plus @ W1 @ in 1)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");


############################################
############################################
$code = <<'CODE';
tr_test \"onep\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fn (W1\ fn (W2\ fn (W3\ cond (equal @ in 1 @ W1) W2 W3)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"is_sym\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fn (W1\ fn (W2\ fn (W3\ equal @ (W1 @ W2 @ W3) @ (W1 @ W3 @ W2))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"fib\" T.

CODE
$ans = <<'ANS';

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"map\" T.

CODE
$ans = <<'ANS';

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"mem\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fixpt (W1\ fn (W2\ fn (W3\ cond (nullp @ W3) false (cond (and @ (consp @ W3) @ (equal @ (car @ W3) @ W2)) truth (W1 @ W2 @ (cdr @ W3))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"fact\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fixpt (W1\ fn (W2\ fn (W3\ cond (equal @ W2 @ in 0) W3 (W1 @ (minus @ W2 @ in 1) @ (times @ W2 @ W3)))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");

############################################
############################################
$code = <<'CODE';
tr_test \"app\" T.

CODE
$ans = <<'ANS';

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");


############################################
############################################
$code = <<'CODE';
tr_test \"gcd\" T.

CODE
$ans = <<'ANS';

The answer substitution:
T = fixpt (W1\ fn (W2\ fn (W3\ cond (equal @ in 1 @ W2) (in 1) (cond (greater @ W3 @ W2) (W1 @ W3 @ W2) (cond (equal @ W2 @ W3) W2 (W1 @ (minus @ W2 @ W3) @ W3))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tr_test");
