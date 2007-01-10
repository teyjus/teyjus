%{
#include "types.h"
#include "y.tab.h"
#include <string.h> 

static int commentLev = 0;
%}

LETTER [A-Za-z]
DIGIT  [0-9]
SYMBOL "_"|"+"|"-"|"*"|"/"|"!"|"~"|"@"|"$"|"%"|"^"|"&"|"*"|"<"|">"|"="|"'"|";"|":"|","

ID     ({LETTER}|{SYMBOL})({LETTER}|{DIGIT}|{SYMBOL})*
NUM    {DIGIT}+
WSPACE [ \t]+
STRING [^*/]+

%x COMMENT COMMENT2

%%
<INITIAL,COMMENT>"\n"      {continue;                                      }
<INITIAL>"KIND"            {return KIND;                                   }
<INITIAL>"CONST"           {return CONST;                                  }
<INITIAL>"TYPE SKEL"       {return TYSKEL;                                 }
<INITIAL>"TYPE"            {return TYPE;                                   }
<INITIAL>"EMPTY TYPE"      {return EMPTYTYPE;                              }
<INITIAL>"->"              {return TYARROW;                                }
<INITIAL>"@"               {return TYAPP;                                  }
<INITIAL>"["               {return LBRACKET;                               }
<INITIAL>"]"               {return RBRACKET;                               }
<INITIAL>"("               {return LPAREN;                                 }
<INITIAL>")"               {return RPAREN;                                 }
<INITIAL>"t,"              {return COMMA;                                  }
<INITIAL>"#"               {return POUND;                                  }
<INITIAL>"EMPTY"           {return EMPTY;                                  }
<INITIAL>"LOGIC SYMBOL"    {return LSSYMB;                                 }
<INITIAL>"LS_START"        {return LSSTART;                                }
<INITIAL>"LS_END"          {return LSEND;                                  }
<INITIAL>"PRED SYMBOL"     {return PREDSYMB;                               }
<INITIAL>"PRED_START"      {return PREDSTART;                              }
<INITIAL>"PRED_END"        {return PREDEND;                                }
<INITIAL>"TRUE"            {return TRUE;                                   }
<INITIAL>"FALSE"           {return FALSE;                                  }
<INITIAL>{WSPACE}          {continue;                                      }
<INITIAL>"/%"              {commentLev = 1; BEGIN(COMMENT); continue;      }
<INITIAL>"/*"              {BEGIN(COMMENT2); continue;                     }
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

.                          {return ERROR;                                  }
