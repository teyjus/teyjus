use lib '../lib';
use strict;
use Test::More tests => 5;

my $TJSIM = "../../tjsim";
my $PATH = "--path hocompiler/";
my $MODULE = "realsparc";
my $code;
my $ans;
my $PT = "hocompiler";


############################################
############################################
$code = <<'CODE';
tigcompile "hocompiler/test3.bob" "hocompiler/test3.s".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 49
Parsing...

Assembly code written to file.

ANS
same_answers( `$TJSIM -b $PATH --solve '$code' $MODULE\n diff $PT/test3.s $PT/test3_st.s`, $ans,"tigcompile");
#diff $PATH/test3.s $PATH/test3_st.s;

############################################
$code = <<'CODE';
tigcompile "hocompiler/test4.bob" "hocompiler/test4.s".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 18
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve '$code' $MODULE\n diff $PT/test4.s $PT/test4_st.s`, $ans,"tigcompile");
#diff $PATH/test4.s $PATH/test4_st.s;

############################################
$code = <<'CODE';
tigcompile "hocompiler/test5.bob" "hocompiler/test5.s".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 25
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve '$code' $MODULE\n diff $PT/test5.s $PT/test5_st.s`, $ans,"tigcompile");
#diff $PATH/test5.s $PATH/test5_st.s;

############################################
$code = <<'CODE';
tigcompile "hocompiler/test6.bob" "hocompiler/test6.s".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 15
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve '$code' $MODULE\n diff $PT/test6.s $PT/test6_st.s`, $ans,"tigcompile");
#diff $PATH/test6.s $PATH/test6_st.s;

############################################
$code = <<'CODE';
tigcompile "hocompiler/test7.bob" "hocompiler/test7.s".

CODE
$ans = <<'ANS';

Scanning for tokens...
Tokenizer stopped after line 40
Parsing...

Assembly code written to file.


ANS
same_answers( `$TJSIM -b $PATH --solve '$code' $MODULE\n diff $PT/test7.s $PT/test7_st.s`, $ans,"tigcompile");
#diff $PATH/test7.s $PATH/test7_st.s;