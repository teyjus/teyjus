use lib '../lib';
use strict;
use Test::More tests => 19;

my $TJSIM = "../../tjsim";
my $PATH = "-p pcf_mono/";
my $MODULE = "mono_test";
my $code;
my $ans;


############################################
############################################
$code = <<'CODE';
mono_test \"successor\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num --> num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"onep\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num --> _T1 --> _T1 --> _T1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"is_sym\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = (_T1 --> _T1 --> _T2) --> _T1 --> _T1 --> bool

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"fib\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num --> num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"map\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = (_T1 --> _T2) --> lst _T1 --> lst _T2

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"mem\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = _T1 --> lst _T1 --> bool

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"fact\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num --> num --> num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");
############################################
############################################

$code = <<'CODE';
mono_test \"app\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = lst _T1 --> lst _T1 --> lst _T1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"gcd\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num --> num --> num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"ex1\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"ex2\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"ex3\" Ty.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"ex4\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = _T1 --> _T2 --> _T2

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");


############################################
############################################
$code = <<'CODE';
mono_test \"ex5\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = num

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"ex6\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = _T1 --> _T1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"i\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = _T1 --> _T1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"k\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = _T1 --> _T2 --> _T1

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"s\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = (_T1 --> _T2 --> _T3) --> (_T1 --> _T2) --> _T1 --> _T3


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

############################################
############################################
$code = <<'CODE';
mono_test \"comp\" Ty.

CODE
$ans = <<'ANS';

The answer substitution:
Ty = (_T1 --> _T2) --> (_T3 --> _T1) --> _T3 --> _T2


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"mono_test");

