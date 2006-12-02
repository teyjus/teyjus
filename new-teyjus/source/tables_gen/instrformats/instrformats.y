%{
#include <stdio.h>
#include <stdlib.h>
#include "instrgen-c.h"

int yywrap() {return 1;}

void yyerror(const char* str)
{
    printf("%s\n", str);
}
 
%}

%union{
    char*    name;
    char*    text;
    struct
    {
        int    ival;
        char*  sval;
    }        isval;
}

%token          OPTYPES INSTRCAT INSTRUCTIONS OPCODE MAXOPERAND  
                CALL_I1_LEN SEMICOLON ERROR LBRACKET RBRACKET
%token <name>   ID
%token <isval>  NUM
%token <text>   STRING

%start          instr_format
%type  <name>   operand_name operand_tname operand_type instr_name instr_cat 
                instr_head instr_length
%type  <text>   comments         
%type  <isval>  max_operand opcode
%%
    
instr_format   : operands instrcats instructions

operands       : OPTYPES operand_decs opcode_type 
                 { genOpsH();}
               ;

operand_decs   : operand_dec operand_decs
               | operand_dec_last
               ;

operand_dec    : operand_name operand_tname operand_type comments
                 { genOpTypes($1, $2, $3, $4, 0); }
               | operand_name comments
                 { genOpTypes($1, NULL, NULL, $2, 0); }       
               ;

operand_dec_last :  operand_name operand_tname operand_type comments
                    { genOpTypes($1, $2, $3, $4, 1); }   
                 |  operand_name comments
                    { genOpTypes($1, NULL, NULL, $2, 1); } 
                 ;

operand_name   : ID { $$ = $1; }
               ;

operand_tname  : ID { $$ = $1; }
               ;

operand_type   : ID { $$ = $1; }
               ;

comments       : STRING {$$ = $1; }
               ;

opcode_type    : OPCODE ID { genOpCodeType($2); }
               ;

instrcats      : INSTRCAT max_operand instrcat_decs CALL_I1_LEN NUM
                 { genInstrCatH($5.sval); genInstrCatC($2.sval);}
               ;

max_operand    : MAXOPERAND NUM   { $$ = $2; }
               ;

instrcat_decs  : instrcat_dec instrcat_decs 
               | instrcat_dec_last
               ;

instrcat_dec   : ID LBRACKET instr_format RBRACKET instr_lengths
                 { genOneInstrCatH($1, 0); genOneInstrCatC($1, 0); } 
               ;

instrcat_dec_last : ID LBRACKET instr_format RBRACKET instr_lengths
                    { genOneInstrCatH($1, 1); genOneInstrCatC($1, 1); } 
               ;

instr_format  : oneOp instr_format
              | lastOp
              ;  

oneOp         : ID { genInstrFormat($1, 0); }
              ;

lastOp        : ID { genInstrFormat($1, 1); }
              ;
              

instr_lengths : instr_len SEMICOLON instr_lengths
              | instr_len
              ;

instr_len     : ID NUM  { genInstrLength($1, $2.sval);}
              ;


instructions  : instr_head instrs
                { genInstrH($1); genInstrC(); genSimDispatch();}
              ;

instr_head    : INSTRUCTIONS NUM  
                { initInstrC($2.ival); 
                  initSimDispatch($2.ival); 
                  $$ = $2.sval; 
                }
              ;


instrs        : instr SEMICOLON instrs
              | last_instr
              ;

instr         : comments opcode instr_name instr_cat instr_length
                { genOneInstrH($1, $2.sval , $3);
                  genOneInstrC($2.ival, $3, $4, $5, 0);
                  genOneSimDispatch($2.ival, $3, 0);
                }
              | opcode instr_name instr_cat instr_length
                { genOneInstrH(NULL, $1.sval , $2);
                  genOneInstrC($1.ival, $2, $3, $4, 0);
                  genOneSimDispatch($1.ival, $2, 0);   
                }
              ;

last_instr    : comments opcode instr_name instr_cat instr_length
                { genOneInstrH($1, $2.sval , $3);
                  genOneInstrC($2.ival, $3, $4, $5, 1);
                  genOneSimDispatch($2.ival, $3, 1); 
                }
              |  opcode instr_name instr_cat instr_length
                { genOneInstrH(NULL, $1.sval , $2);
                  genOneInstrC($1.ival, $2, $3, $4, 1);
                  genOneSimDispatch($1.ival, $2, 1);                
                }
              ;
 
opcode        : NUM { $$ = $1; }
              ;

instr_name    : ID { $$ = $1; }
              ;

instr_cat     : ID { $$ = $1; }
              ;

instr_length  : ID { $$ = $1; }
              ; 


%%

extern FILE* yyin;

int main(argc, argv)
    int argc;
    char * argv[];
{
    if (sizeof(void*) == 8) yyin = fopen("instrformats_64.in", "r");
    else yyin = fopen("instrformats_32.in", "r");

    if (yyin){
        yyparse();
        fclose(yyin);
        spitCInstructionsH();
        spitCInstructionsC();
        spitSimDispatch();
        printf("Instruction files genereated\n");
    } else {
        printf("cannot open input file \n");
    }
    return 0;
}
