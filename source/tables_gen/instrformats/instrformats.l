%{
#include "y.tab.h"
#include <string.h>
#include <stdlib.h>

static int commentLev = 0;
%}

LETTER [A-Za-z]
DIGIT  [0-9]
SYMBOL "_"|"+"|"-"|"*"|"/"|"!"|"~"|"@"|"$"|"%"|"^"|"&"|"*"|"<"|">"|"="|"'"|":"|","

ID     ({LETTER}|{SYMBOL})({LETTER}|{DIGIT}|{SYMBOL})*
NUM    {DIGIT}+
WSPACE [ \t]+
STRING [^*/]+
STRING2 [^}]+

%x COMMENT COMMENT2 INCLUDE

%%
<INITIAL,COMMENT>"\n"      {continue;                                      }
<INITIAL>";"               {return SEMICOLON;                              }
<INITIAL>"["               {return LBRACKET;                               }
<INITIAL>"]"               {return RBRACKET;                               }
<INITIAL>"OPERAND TYPES"   {return OPTYPES;                                }
<INITIAL>"OPCODE"          {return OPCODE;                                 }
<INITIAL>"INSTR CATEGORY"  {return INSTRCAT;                               }
<INITIAL>"MAX OPERAND"     {return MAXOPERAND;                             }
<INITIAL>"CALL_I1_LEN"     {return CALL_I1_LEN;                            }
<INITIAL>"INSTRUCTIONS"    {return INSTRUCTIONS;                           }
<INITIAL>{WSPACE}          {continue;                                      }  
<INITIAL>"/%"              {commentLev = 1; BEGIN(COMMENT); continue;      }
<INITIAL>"/*"              {BEGIN(COMMENT2); continue;                     }
<INITIAL>"{"               {BEGIN(INCLUDE); continue;                      }
<INITIAL>{ID}              {yylval.name = strdup(yytext); return ID;       }
<INITIAL>{NUM}             {yylval.isval.ival = atoi(yytext); 
                            yylval.isval.sval = strdup(yytext); 
                            return NUM;                                    }
<COMMENT2>"*/"             {BEGIN(INITIAL); continue;                      }
<COMMENT2>{STRING}         {yylval.text = strdup(yytext); return STRING;   }

<COMMENT>[^%/\n]+          {continue;                                      }
<COMMENT>"/%"              {commentLev++; continue;                        }
<COMMENT>"%/"              {commentLev--;
                            if (!commentLev) BEGIN(INITIAL); continue;     }
<INCLUDE>"}"               {BEGIN(INITIAL); continue;                      }
<INCLUDE>{STRING2}         {yylval.text = strdup(yytext); return STRING2;  }
.                          {return ERROR;                                  }
