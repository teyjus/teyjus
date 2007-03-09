#ifndef OP_H
#define OP_H

//fixity type
typedef enum {
    OP_INFIX, OP_INFIXL, OP_INFIXR, OP_PREFIX, OP_PREFIXR, OP_POSTFIX,
    OP_POSTFIXL, OP_NONE
} OP_Fixity;

typedef enum {
    OP_PREC, OP_PREC_NAME
} OP_PrecTypeCat;

//precedence type
typedef struct
{
    OP_PrecTypeCat cat;
    union
    {
        int    prec;
        char*  name;
    } data;
} OP_Prec;

OP_Prec OP_mkPrecMin1();
OP_Prec OP_mkPrecMin2();
OP_Prec OP_mkPrec(int prec);
OP_Prec OP_mkPrecMax();

int     OP_precIsMax(OP_Prec prec);


//code info type
typedef int OP_Code;

OP_Code OP_mkCodeInfoNone();
OP_Code OP_mkCodeInfo(int ind);

int     OP_codeInfoIsNone(OP_Code code);

#endif 
    
