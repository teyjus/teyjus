use lib '../lib';
use strict;
use Test::More tests => 23;

my $TJSIM = "../../tjsim";
my $PATH = "--path typeinf/";
my $MODULE = "mlt";
my $code;
my $ans;

############################################
############################################
$code = <<'CODE';
tryonce_ans (abs x\ (app op zero)) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ arr W1 integer)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs f\ (app f zero)) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ arr (arr integer W2) W2))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");


############################################
$code = <<'CODE';
tryonce_ans (abs f\ (abs x\ (app f x))) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ arr (arr W2 W3) (arr W2 W3))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (abs f\ (app f x))) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ arr W1 (arr (arr W1 W3) W3))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs f\ (abs g\ (abs x\ (app g (app f x))))) T.


CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ arr (arr W3 W5) (arr (arr W5 W4) (arr W3 W4)))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (abs f\ (app (app f x) (app (app f x) x)))) T.


CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ arr W3 (arr (arr W3 (arr W3 W3)) W3)))))))
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans  (abs f\ (abs x\ (abs y\ (app (app f (app op x)) (app op y))))) T.

CODE
$ans = <<'ANS';


The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ all (W8\ all (W9\ arr (arr W9 (arr W6 W4)) (arr W9 (arr W6 W4)))))))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs y\ (let (x\ (abs z\x)) (app succ y))) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ arr integer (arr W2 integer)))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (let (x\ (app x x)) (abs y\y)) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ arr W2 W2))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (let (x\ (let (z\ (app z (app (app x x) (app z z)))) (abs u\u))) (abs y\y)) T. 

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ all (W8\ all (W9\ all (W10\ all (W11\ arr W4 W4)))))))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");


############################################
$code = <<'CODE';
tryonce_ans (let (f\ (app (app (abs d1\ (abs (d2 \ zero))) (app f zero)) (app f (abs x \ x)))) (abs x \ x)) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ all (W8\ all (W9\ all (W10\ integer))))))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (y\x) (app x zero))) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ arr (arr integer W2) (arr integer W2)))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (app op (app op x))) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ arr W5 W5)))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (y\ (app (abs c\c) y)) (app (abs c\c) x))) T.

CODE
$ans = <<'ANS';


The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ arr W7 W7)))))))


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (u\ (let (v\v) (app succ u))) (app op x))) T.

CODE
$ans = <<'ANS';


The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ all (W8\ all (W9\ arr integer integer)))))))))

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");


############################################
$code = <<'CODE';
tryonce_ans (let (u\ (let (v\v) (app op u))) (abs x\x)) T.

CODE
$ans = <<'ANS';

The answer substitution:
T = all (W1\ all (W2\ all (W3\ all (W4\ all (W5\ all (W6\ all (W7\ arr W2 W2)))))))


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (app (abs x\ (app x x)) (abs x\ (app x x))) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs f\ (let (x\ (app f x)) (app op f))) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let y\ (app y y)) (app op2 x)) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (y\ (app x y)) (app op3 x))) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (y\ (app y y)) (app op3 x))) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (abs x\ (let (v\ (app v v)) (abs z\ (app x z)))) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");

############################################
$code = <<'CODE';
tryonce_ans (let (u\ (let (v\v) (app succ u))) (abs x\x)) T.

CODE
$ans = <<'ANS';
ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE\n`, $ans,"tryonce_ans");
############################################
