use lib '../lib';
use strict;
use Test::More tests => 5;

my $TJSIM = "../../tjsim";
my $PATH = "--path hocompiler/";
my $MODULE = "realsparc";
my $code;
my $ans;
my $PT = "hocompiler";
my $DIFF = "diff --strip-trailing-cr";

if($^O eq 'openbsd'){{
    $DIFF = "gdiff --strip-trailing-cr";
}}



############################################
############################################
$code = <<'CODE';
tigcompile \"hocompiler/test3.bob\" \"hocompiler/test3.s-acc\".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 49
Parsing...

Assembly code written to file.

ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE` . `$DIFF $PT/test3.s-exp $PT/test3.s-acc`, $ans,"tigcompile");

############################################
$code = <<'CODE';
tigcompile \"hocompiler/test4.bob\" \"hocompiler/test4.s-acc\".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 18
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE` . `$DIFF $PT/test4.s-exp $PT/test4.s-acc`, $ans,"tigcompile");

############################################
$code = <<'CODE';
tigcompile \"hocompiler/test5.bob\" \"hocompiler/test5.s-acc\".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 25
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE` . `$DIFF $PT/test5.s-exp $PT/test5.s-acc`, $ans,"tigcompile");

############################################
$code = <<'CODE';
tigcompile \"hocompiler/test6.bob\" \"hocompiler/test6.s-acc\".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 15
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE` . `$DIFF $PT/test6.s-exp $PT/test6.s-acc`, $ans,"tigcompile");

############################################
$code = <<'CODE';
tigcompile \"hocompiler/test7.bob\" \"hocompiler/test7.s-acc\".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 40
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve "$code" $MODULE` . `$DIFF $PT/test7.s-exp $PT/test7.s-acc`, $ans,"tigcompile");
