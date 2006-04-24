(****************************************************************************
*
****************************************************************************)

{
open Parser

let maxStringLength = ref 257

let commentLev = ref 0
let commentPos = ref 0

let strPos = ref 0
let escapePos = ref 0
let strErr = ref false
let quotedid = ref false

let charPos = ref 1
let currentString = ref ""

(**********************************************************************
*addChar:
*	Add a character to the current string.
**********************************************************************)
let addChar = fun s ->
	if(!stringError = false)
		if(size(!currentString) < maxStringLength - 1) then
			(currentString := !currentString ^ s;
			())
		else
			(ErrorMsg.Warning("Maximum string/id length exceeded; truncating to " ^ (intToString maxStringLength) ^ " characters");
			())
	else
		()

(**********************************************************************
*addHex:
*	This *should* convert the given string into a character by interpreting
*	it as either 1 or 2 hexadecimal characters.
**********************************************************************)
let addHex = fun s ->
	s

(**********************************************************************
*addOctal:
*	This *should* convert the given string into a character by interpreting
*	it as either 1 or 3 octal characters.
**********************************************************************)
let addOctal = fun s ->
	s

(**********************************************************************
*addControl:
*	This *should* convert the given string into a character.
**********************************************************************)
let addControl = fun s ->
	s

}

let DIGIT = ['0'-'9']
let OCTAL = ['0'-'7']
let HEX = ['0'-'9' 'A'-'F' 'a'-'f']
let SCHAR = ['+' '-' '*' '/' '^' '<' '>' '=' '`' '\'' '?' '@' '#' '$' '&' '!' '_' '~']
let SCHAR1 = ['+' '-' '/' '^' '<' '>' '=' '`' '\'' '?' '@' '#' '$' '&' '!' '_' '~']
let SCHAR2 = ['+' '-' '*' '^' '<' '>' '=' '`' '\'' '?' '@' '#' '$' '&' '!' '~']
let FCHAR = [' ' '\t' '\x0b' '\x0d']
let PCHAR = ['\040' '!'-'&' '(' '[' ']'-'~']
let LCASE = ['a' - 'z']
let UCASE = ['A' - 'Z']
let IDCHAR = (LCASE|UCASE|DIGIT|SCHAR)
let IDCHAR1 = (LCASE|UCASE|DIGIT|SCHAR1)
let WSPACE = [' ' '\011' '\013' '\015']+
let NUM = DIGIT+


(*
%{
   if (P_FirstToken)
     { P_FirstToken = FALSE;
       switch (P_parseState) {
          case PARSEMOD: return MODSTART;
          case PARSESIG: return SIGSTART;
          case PARSETERM: return TERMSTART;
       }
     }

   if (P_LastToken)
     { P_LastToken = FALSE; yywrap(); return EOF; }
%}
*)

rule inital = parse
|	WSPACE				{initial lexbuf}
|	"\n"					{ErrorMsg.newLine ; continue}

|	"module"			{MODULE}
|	"end"					{END}
|	"import"			{IMPORT}
|	"accumulate"	{ACCUMULATE}
|	"accum_sig"		{ACCUMSIG}
|	"use_sig"			{USESIG}
|	"local"				{LOCAL}
|	"localkind"		{LOCALKIND}
|	"closed"			{CLOSED}
|	"sig"					{SIG}
|	"kind"				{KIND}
|	"type"				{TYPE}
|	"exportdef"		{EXPORTDEF}
|	"useonly"			{USEONLY}
|	"infixl"			{INFIXL}
|	"infix"				{INFIX}
|	"infixr"			{INFIXR}
|	"prefix"			{PREFIX}
|	"prefixr"			{PREFIXR}
|	"postfix"			{POSTFIX}
|	"postfixl"		{POSTFIXL}
|	":-"					{COLONDASH}
|	"=>"					{IMPLIES}
|	"\\"					{INFIXLAMBDA}
|	"->"					{TYARROW}
|	"!"						{CUT}

|	"pi"					{PI}
|	"sigma"				{SIGMA}
|	","						{COMMA}
|	";"						{SEMICOLON}
|	"&"						{AMPAND}
|	"/"						{RDIVIDE}
|	"nil"					{NILLIST}
|	"::"					{LISTCONS}
|	"="						{EQUAL}

|	"+"						{PLUS}
|	"-"						{MINUS}
|	"*"						{TIMES}
|	"<"						{LESS}
|	"=<"					{LEQ}
|	">"						{GTR}
|	">="					{GEQ}
|	"~"						{UMINUS}

|	"."						{PERIOD}
|	"("						{LPAREN}
|	")"						{RPAREN}
|	"["						{LBRACK}
|	"]"						{RBRACK}
|	":"						{COLON}
|	"|"						{VBAR}

|	(NUM?"."NUM) as num	{REALLIT(float_of_string(num)}
|	NUM as num						{INTLIT(int_of_string(num))}

|	UCASE IDCHAR* as name				{UPCID{name=name, kind=P.CVID}}
|	LCASE IDCHAR* as name				{ID{name=name, kind=P.ConstID}}
|	(("/"(IDCHAR1 IDCHAR*))|(SCHAR2 IDCHAR*)) as name {SYID{name=name, kind=P.ConstID}}

|	"_"              {VID{name=yytext, kind=P.AVID}}
|	"_" IDCHAR+     	{VID{name=yytext, kind=P.VID}}

|	"\""             {stringstate lexbuf; }

|	"%"              {comment1 lexbuf}

|	"/*"             {commentPos = charPos; commentLev=1; comment2 lexbuf}

(**********************************************************************
*stringstate:
*	This state handles reading a quoted string.
**********************************************************************)
and stringstate = parse
|	['^' '"' '\\' '\n']+	{stringstate lexbuf}
|	'"'										{STRLIT(!currentString)}
|	'\n'					{ErrorMsg.error strPos "Error: String literal ended with newline";
								ErrorMsg.newLine(); STRLIT(!currentString)}
|	"\\a"					{addChar("\a"); stringstate lexbuf}
|	"\\b"					{addChar("\b"); stringstate lexbuf}
|	"\\t"					{addChar("\t"); stringstate lexbuf}
|	"\\n"					{addChar("\n"); stringstate lexbuf}
|	"\\v"					{addChar("\v"); stringstate lexbuf}
|	"\\f"					{addChar("\f"); stringstate lexbuf}
|	"\\r"					{addChar("\r"); stringstate lexbuf}
|	"\\e"					{addChar("\x1b"); stringstate lexbuf}
|	"\\d"					{addChar("\x7f"); stringstate lexbuf}
|	"\\\\"				{addChar("\\"); stringstate lexbuf}
|	"\\\""				{addChar("\""); stringstate lexbuf}
|	"\"\""				{addChar("\""); stringstate lexbuf}

|	"\\^"['@'-'z'] as text					{addControl(text); stringstate lexbuf}
|	"\\" OCTAL as text							{addOctal(text); stringstate lexbuf}
|	"\\" OCTAL OCTAL OCTAL as text	{addOctal(text); stringstate lexbuf}
|	"\\x" HEX as text								{addHex(text); stringstate lexbuf}
|	"\\x" HEX HEX as text						{addHex(text); stringstate lexbuf}

|	"\\x" _					{EM_error(charPos,"Error: Illegal hex character specification");
									stringstate lexbuf}
|	"\\" FCHAR			{strflush1 lexbuf}
|	"\\\n"					{ErrorMsg.newLine(); strflush1 lexbuf}
|	"\\c"						{strflush2 lexbuf}
|	"\\" _					{EM_error(charPos,"Error: Illegal escape character in string");
									stringstate lexbuf}

and strflush1 = parse
|	FCHAR+		{strflush1 lexbuf}
|	"\\"			{strflush1 lexbuf}
| _ as text	{EM.error(escapePos, "Error: Unterminated string escape sequence");
						addChar(text); adjust();
						stringstate lexbuf}

and strflush2 = parse
|	FCHAR+		{strflush2 lexbuf}
|	_					{addChar(text); stringstate lexbuf}

and comment1 = parse
|	[^ '\n']+ 			{comment1 lexbuf}
|	"\n"						{ErrorMsg.newLine(); initial lexbuf}
|	_ as text				{ErrorMsg.Error("Illegal character " ^ text ^ " in input");
									comment1 lexbuf}

and comment2 = parse
|	[^ '*' '/' '\n']+		{comment2 lexbuf}
|	"/*"				{commentLev := !commentLev + 1; comment2 lexbuf}
|	"*/"				{commentLev := !commentLev - 1;
							if(!commentLev = 0) then
								initial lexbuf
							else
								comment2 lexbuf}
|	"/*"				{comment2 lexbuf}
|	_ as text		{ErrorMsg.Error("Illegal character " ^ text ^ " in input");
							comment2 lexbuf}
