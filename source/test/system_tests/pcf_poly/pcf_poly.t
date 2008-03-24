use lib '../lib';
use strict;
use Test::More tests => 19;

my $TJSIM = "../../tjsim";
my $PATH = "--path pcf_poly/";
my $MODULE = "poly_test";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
poly_test \"successor\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = c (num --> num)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"onep\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ c (num --> W1 --> W1 --> W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"is_sym\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ c ((W2 --> W2 --> W1) --> W2 --> W2 --> bool)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"fib\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = c (num --> num)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"map\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ c ((W2 --> W1) --> lst W2 --> lst W1)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"mem\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ c (W1 --> lst W1 --> bool))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"fact\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = c (num --> num --> num)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"app\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ c (lst W1 --> lst W1 --> lst W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"gcd\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = c (num --> num --> num)

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex1\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex2\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex3\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex4\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ c (W1 --> W2 --> W2)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex5\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = c num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"ex6\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ c (W1 --> W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"i\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ c (W1 --> W1))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"k\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ c (W2 --> W1 --> W2)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"s\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ all (W3\ c ((W3 --> W2 --> W1) --> (W3 --> W2) --> W3 --> W1))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");

############################################
############################################
$code = <<'CODE';
poly_test \"comp\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = all (W1\ all (W2\ all (W3\ c ((W2 --> W1) --> (W3 --> W2) --> W3 --> W1))))


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"poly_test");
###########################################
###########################################
