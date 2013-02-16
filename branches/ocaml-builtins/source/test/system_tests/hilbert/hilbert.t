use lib '../lib';
use strict;
use Test::More tests => 7;

my $TJSIM = "../../tjsim";
my $PATH = "--path hilbert/";
my $MODULE = "hilbert";
my $code;
my $ans;

################
############################################
############################################
$code = <<'CODE';
test 1.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################
$code = <<'CODE';
test 2.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################
$code = <<'CODE';
test 3.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################

$code = <<'CODE';
test 4.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################
$code = <<'CODE';
test 5.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################
$code = <<'CODE';
test 6.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################
$code = <<'CODE';
test 7.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################