use lib '../lib';
use strict;
use Test::More tests => 1;

my $TJSIM = "../../tjsim";
my $PATH = "--path handbook_progs_curry/";
my $MODULE = "curry_test";
my $code;
my $ans;

############################################
############################################

$code = <<'CODE';
test 1 F.
CODE
$ans = <<'ANS';

The answer substitution:
F = fix (W1\ abs (W2\ abs (W3\ _T1 W2 W3 truth (W4\ W5\ app (app W1 W4) W5))))

The remaining disagreement pairs list:
<_T1 (fst #1) (snd #1) (prp #1) (W1\ W2\ app #2 (pr W1 W2)), cond (&& (prp #1) (eq (fst #1) (c 0))) (snd #1) (cond (prp #1) (app #2 (pr (minus (fst #1) (c 1)) (times (fst #1) (snd #1)))) err)>

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"test");
############################################
############################################