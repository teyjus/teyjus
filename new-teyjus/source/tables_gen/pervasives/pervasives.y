%{
#include <stdio.h>
#include <stdlib.h>
#include "types.h"
#include "pervgen-c.h"

int yywrap() {return 1;}

void yyerror(const char* str)
{
    printf("%s\n", str);
}

static int tySkelInd = 0;
 static int i = 0;
 static int j = 0;
 
%}

%union{
    char*            name;  
    char*            text;
    struct 
    {
        int   ival;
        char* sval;
    }                isval;
    Type             tyval;
    TypeList         tylval;
}

%token          KIND CONST ERROR  TYARROW TYAPP LBRACKET RBRACKET LPAREN RPAREN
                COMMA POUND TYSKEL TYPE EMPTYTYPE EMPTY LSSYMB LSSTART LSEND
                PREDSYMB PREDSTART PREDEND
%token <name>   ID
%token <isval>  NUM
%token <text>   STRING

%start          pervasive_decls
%type  <tyval>  tyskel   tyskel1   tyskel2        
%type  <tylval> tyskel_list
%type  <name>   kind_name kind_ind_name const_name const_ind_name fixity ls_type
%type  <isval>  kind_num kind_tab_ind kind_arity const_num tyskel_num ty_index 
                const_tab_ind tesize tyskel_tab_ind prec ls_num pred_num
%type  <text>   comments

%%

pervasive_decls     : kind const_tyskel 
                    ;

kind                : kind_header kind_decls
                      {genKindH(1); genKindC();}
                    | kind_header 
                      {genKindH(0); genKindC();}
                    ;

kind_header         : KIND  kind_num
                      {kindInfoInit($2.ival); genNumKinds($2.sval);}
                    ;

kind_num            : NUM   {$$ = $1;}
                    ;

kind_decls          : kind_decl kind_decls
                    | kind_decl
                    ;

kind_decl           : kind_tab_ind kind_name kind_ind_name kind_arity
                      {genKindIndices($1.ival, $3, $1.sval, NULL);
                       genKindData($1.ival, $2, $4.sval, NULL); }
                    | comments kind_tab_ind kind_name kind_ind_name kind_arity 
                      {genKindIndices($2.ival, $4, $2.sval, $1);
                       genKindData($2.ival, $3, $5.sval, $1); }		      
                    ;
        
comments            : STRING {$$ = $1;}
                    ;
       
kind_tab_ind        : NUM  {$$ = $1;}
                    ;

kind_name           : ID   {$$ = $1;}
                    ;
kind_ind_name       : ID   {$$ = $1;}


kind_arity          : NUM  {$$ = $1;}
                    ;



const_tyskel        : const_tyskel_header const_tyskel_decls const_property
                      {genTySkelH(); genTySkelC(); 
                       genConstH();  genConstC();  }        
                    ; 

const_tyskel_header : CONST const_num TYSKEL tyskel_num
                      {genNumConsts($2.sval); constInfoInit($2.ival);
                       genNumTySkels($4.sval); tySkelInfoInit($4.ival);  
                      }
                    ;

const_num           : NUM { $$ = $1; }
                    ;
tyskel_num          : NUM { $$ = $1; }
                    ;    


const_tyskel_decls  : const_tyskel_decl const_tyskel_decls
                    | const_tyskel_decl
                    ;

const_tyskel_decl   : tyskel_decl const_decls
                    | EMPTYTYPE empty_consts
                    ;

tyskel_decl         : TYPE tyskel_tab_ind tyskel
                      {tySkelInd = $2.ival;
                       genTySkels($2.ival, $3, NULL); }
                    | TYPE comments tyskel_tab_ind tyskel
                      {tySkelInd = $3.ival; 
                       genTySkels($3.ival, $4, $2);   } 
                    ;

tyskel_tab_ind      : NUM { $$ = $1;};


tyskel              : tyskel1 TYARROW tyskel
                      { $$ = mkArrowType($1, $3); }
                    | tyskel1
                      { $$ = $1; }                   
                    ; 

tyskel1             : LPAREN TYAPP kind_ind_name NUM LBRACKET tyskel_list 
                      RBRACKET RPAREN
                      { $$ = mkStrType(mkStrFuncType($3,$4.sval), $4.ival, $6);}
                    | tyskel2
                      { $$ = $1; }
                    ;          

tyskel2             : kind_ind_name
                      { $$ = mkSortType($1); }
                    | ty_index 
                      { $$ = mkSkVarType($1.sval); }
                    | LPAREN tyskel RPAREN
                      { $$ = $2; }
                    ;

tyskel_list         : tyskel COMMA tyskel_list
                      { $$ = addItem($1, $3); }
                    | tyskel 
                      { $$ = addItem($1, NULL); }
                    ;

ty_index            : POUND NUM  {$$ = $2;}    
                    ;

const_decls         : const_decl const_decls
                    | const_decl
                    ;

const_decl          : const_tab_ind const_name const_ind_name tesize prec fixity
                      { genConstIndices($1.ival, $3, $1.sval, NULL);
                        genConstData($1.ival, $2, $4.sval, $5.sval, $6, 
                                     tySkelInd, NULL);
                      }
                    | comments const_tab_ind const_name const_ind_name tesize 
                      prec fixity
                      { genConstIndices($2.ival, $4, $2.sval, $1);
                        genConstData($2.ival, $3, $5.sval, $6.sval, $7, 
                                     tySkelInd, $1);                        
                      }
                    ;

empty_consts        : empty_const empty_consts
                    | empty_const
                    ;

empty_const         : const_tab_ind EMPTY
                      { genConstEmptyIndices($1.ival); 
                        genConstEmptyData($1.ival);}
                    ;

const_tab_ind       : NUM {$$ = $1;}
                    ;

const_name          : ID  {$$ = $1;}
                    ;
const_ind_name      : ID  {$$ = $1;}
                    ;

tesize              : NUM {$$ = $1;}
                    ;

prec                : NUM {$$ = $1;}
                    ;

fixity              : ID  {$$ = $1;}
                    ;

const_property      : logic_symbol pred_symbol
                    ;

logic_symbol        : ls_header ls_range ls_types
                    ;

ls_header           : LSSYMB ls_num       {lsInfoInit($2.ival); }
                    ;

ls_range            : LSSTART const_ind_name LSEND const_ind_name 
                      { genLSRange($2, $4);}    
                    ;

ls_num              : NUM  {$$ = $1;}
                    ;

ls_types            : ls_type ls_types
                    | ls_type
                    ;

ls_type             : NUM ID  {genLogicSymbTypes($1.ival, $2, $1.sval);}
                    ;

pred_symbol         : pred_header pred_range 
                    ;

pred_header         : PREDSYMB pred_num      
                      {if ($2.ival == 0) {
                       fprintf(stderr, 
                               "The number of predicate symbols cannot be 0\n");
                       exit(1);
                       } 
                      }
                    ;

pred_range          : PREDSTART const_ind_name PREDEND const_ind_name
                      { genPREDRange($2, $4);} 
                    ;

pred_num            : NUM  {$$ = $1;}
                    ;




    


%%

extern FILE* yyin;

int main(argc, argv)
    int argc;
    char * argv[];
{
    yyin = fopen(argv[1], "r");
    if (yyin){
        yyparse();
        fclose(yyin);
        spitCPervasivesH();
        spitCPervasivesC();
        printf("Pervasive files genereated\n");
    } else {
        printf("cannot open input file %s\n", argv[1]);
    }
    return 0;
}
