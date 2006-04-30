(****************************************************************************
*
****************************************************************************)

{
open Parsing
open Lplex

let maxStringLength = 257

let commentLev = ref 0
let commentPos = ref 0

let strPos = ref 0
let escapePos = ref 0
let strErr = ref false
let quotedid = ref false

let currentString = ref ""

(**********************************************************************
*currentPos:
*	Returns current character position.
**********************************************************************)
let currentPos = function () ->
	0

(**********************************************************************
*addChar:
*	Add a character to the current string.
**********************************************************************)
let addChar = function s ->
	if (String.length(!currentString) < (maxStringLength - 1)) then
		(currentString := !currentString ^ s;
		())
	else
		(ErrorMsg.warning (!strPos) ("Maximum string/id length exceeded; truncating to " ^ (string_of_int maxStringLength) ^ " characters");
		())

(**********************************************************************
*addHex:
*	This *should* convert the given string into a character by interpreting
*	it as either 1 or 2 hexadecimal characters.
**********************************************************************)
let addHex = fun s ->
	(addChar (String.make 1 (Char.chr(int_of_string ("0x" ^ s)))))

(**********************************************************************
*addOctal:
*	This *should* convert the given string into a character by interpreting
*	it as either 1 or 3 octal characters.
**********************************************************************)
let addOctal = fun s ->
	(addChar (String.make 1 (Char.chr(int_of_string ("0o" ^ s)))))

(**********************************************************************
*addControl:
*	This *should* convert the given string into a character control.
**********************************************************************)
let addControl = fun s ->
	(addChar (String.make 1 (Char.chr ((Char.code (String.get s 0)) -
		(Char.code '@')))))
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
(* NOTE: Need to somehow set the state using MODSTART, SIGSTART, etc.	*)
%}
*)

rule initial = parse
|	WSPACE				{initial lexbuf}
|	"\n"					{ErrorMsg.newLine (currentPos ()); initial lexbuf}

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

|	(NUM? "." NUM) as num		{REALLIT(float_of_string(num))}
|	NUM as num						{INTLIT(int_of_string(num))}

|	UCASE IDCHAR* as name				{UPCID{name=name, kind=P.CVID}}
|	LCASE IDCHAR* as name				{ID{name=name, kind=P.ConstID}}
|	(("/"(IDCHAR1 IDCHAR*))|(SCHAR2 IDCHAR*)) as name {SYID{name=name, kind=P.ConstID}}

|	"_"              {VID{name=yytext, kind=P.AVID}}
|	"_" IDCHAR+     	{VID{name=yytext, kind=P.VID}}

|	"\""             {stringstate lexbuf; }

|	"%"              {comment1 lexbuf}

|	"/*"             {commentPos := currentPos (); commentLev=1; comment2 lexbuf}

(**********************************************************************
*stringstate:
*	This state handles reading a quoted string.
**********************************************************************)
and stringstate = parse
|	['^' '"' '\\' '\n']+	{stringstate lexbuf}
|	'"'										{STRLIT(!currentString)}
|	'\n'					{ErrorMsg.error strPos "Error: String literal ended with newline";
								(ErrorMsg.newLine (currentPos ())); STRLIT(!currentString)}
|	"\\b"					{addChar("\b"); stringstate lexbuf}
|	"\\t"					{addChar("\t"); stringstate lexbuf}
|	"\\n"					{addChar("\n"); stringstate lexbuf}
|	"\\r"					{addChar("\r"); stringstate lexbuf}
|	"\\\\"				{addChar("\\"); stringstate lexbuf}
|	"\\\""				{addChar("\""); stringstate lexbuf}
|	"\"\""				{addChar("\""); stringstate lexbuf}

|	"\\^"['@'-'z'] as text					{addControl(text); stringstate lexbuf}
|	"\\" OCTAL as text							{addOctal(text); stringstate lexbuf}
|	"\\" OCTAL OCTAL OCTAL as text	{addOctal(text); stringstate lexbuf}
|	"\\x" HEX as text								{addHex(text); stringstate lexbuf}
|	"\\x" HEX HEX as text						{addHex(text); stringstate lexbuf}

|	"\\x" _					{ErrorMsg.error (currentPos ()) "Error: Illegal hex character specification";
									stringstate lexbuf}
|	"\\" FCHAR			{strflush1 lexbuf}
|	"\\\n"					{ErrorMsg.newLine (currentPos ()); strflush1 lexbuf}
|	"\\c"						{strflush2 lexbuf}
|	"\\" _					{ErrorMsg.error (charPos ()) "Error: Illegal escape character in string";
									stringstate lexbuf}

and strflush1 = parse
|	FCHAR+		{strflush1 lexbuf}
|	"\\"			{strflush1 lexbuf}
| _ as text	{ErrorMsg.error(escapePos, "Error: Unterminated string escape sequence");
						addChar(text); adjust();
						stringstate lexbuf}

and strflush2 = parse
|	FCHAR+		{strflush2 lexbuf}
|	_					{addChar(text); stringstate lexbuf}

and comment1 = parse
|	[^ '\n']+ 			{comment1 lexbuf}
|	"\n"						{ErrorMsg.newLine (currentPos ()); initial lexbuf}
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
