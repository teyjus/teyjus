/****************************************************************************/
/*                                                                          */
/* File dataformat.c.                                                       */
/* The header file identifies the low-level representation of data objects  */
/* that are manipulated by the machine, through various structure types.    */ 
/****************************************************************************/
#ifndef DATAFORMAT_C
#define DATAFORMAT_C

#include <math.h>
#include <string.h>
#include "dataformat.h"


/********************************************************************/
/*                                                                  */
/*                         TYPE REPRESENTATION                      */
/*                                                                  */
/********************************************************************/

typedef struct                         //type sort
{
    DF_TY_TAG         tag;
    MEM_KSTTABIND     kindTabIndex;
} DF_TY_SORT;

typedef struct                         //type reference
{
    DF_TY_TAG         tag;      
    DF_TYPE_PTR       target;
} DF_TY_REF;

typedef struct                         //variables in type skeletons
{
    DF_TY_TAG         tag;
    MEM_SKELIND       offset;
} DF_TY_SKVAR;

typedef struct                         //type arrows
{
    DF_TY_TAG         tag;       
    DF_TYPE_PTR       args;
} DF_TY_ARROW;

/*
// an alternative of encoding type arrow 
typedef struct
{
    DF_TY_TAG         tag;
    MEM_TY_ARITY      arity;
    DF_TYPE_PTR       targetAndArgs;
}
*/

typedef struct                         //type functors
{
    MEM_TY_ARITY       arity;
    MEM_KSTTABIND      kindTabIndex;
} DF_TY_FUNC;

typedef struct                         //type structures
{
    DF_TY_TAG         tag;       
    DF_TY_FUNC        *funcAndArgs;
} DF_TY_STR;

/******************************************************************/
/*                      Interface functions                       */
/******************************************************************/

/* TYPE DEREFERENCE */
DF_TYPE_PTR DF_typeDeref(DF_TYPE_PTR typ)
{
    DF_TYPE ty = *typ;
    while ((ty.tag == DF_TY_TAG_REF)){
        DF_TYPE_PTR target = (DF_TYPE_PTR)(ty.dummy);
        if (typ == target) return typ;
        typ = target;
        ty = *typ;
    }
    return typ;
}

/* TYPE RECOGNITION */

BOOLEAN DF_TY_isSort(DF_TYPE_PTR typ)    {return (typ->tag == DF_TY_TAG_SORT); }
BOOLEAN DF_TY_isRef(DF_TYPE_PTR typ)     {return (typ->tag == DF_TY_TAG_REF);  }
BOOLEAN DF_TY_isSkelVar(DF_TYPE_PTR typ) {return (typ->tag == DF_TY_TAG_SKVAR);}
BOOLEAN DF_TY_isArrow(DF_TYPE_PTR typ)   {return (typ->tag == DF_TY_TAG_ARROW);}
BOOLEAN DF_TY_isStr(DF_TYPE_PTR typ)     {return (typ->tag == DF_TY_TAG_STR);  }
BOOLEAN DF_TY_isFreeVar(DF_TYPE_PTR typ)
{
    return ((typ->tag == DF_TY_TAG_REF) && ((DF_TY_REF*)typ)->target == typ);
}

/* TYPE DECOMPOSITION */
DF_TY_TAG DF_TY_tag(DF_TYPE_PTR typ)                     //generic type
{
    return typ -> tag;
}
MEM_KSTTABIND DF_TY_kindTabIndex(DF_TYPE_PTR typ)        //sorts
{
    return ((DF_TY_SORT*)typ) -> kindTabIndex;
}
MEM_SKELIND DF_TY_skelVarIndex(DF_TYPE_PTR typ)          //skel var
{
    return ((DF_TY_SKVAR*)typ) -> offset;
}
DF_TYPE_PTR DF_TY_refTarget(DF_TYPE_PTR typ)             //reference
{
    return ((DF_TY_REF*)typ) -> target;
}
DF_TYPE_PTR DF_TY_arrowArgs(DF_TYPE_PTR typ)             //arrows
{
    return ((DF_TY_ARROW*)typ) -> args;
}
DF_TYPE_PTR DF_TY_strFuncAndArgs(DF_TYPE_PTR typ)        //structures
{
    return (DF_TYPE_PTR)(((DF_TY_STR*)typ)->funcAndArgs);
}
MEM_KSTTABIND DF_TY_strFuncInd(DF_TYPE_PTR typ) 
{//Note typ must refer to funcAndArgs field
    return ((DF_TY_FUNC*)typ)->kindTabIndex;
}
MEM_TY_ARITY DF_TY_strFuncArity(DF_TYPE_PTR typ)
{//Note typ must refer to funcAndArgs field
    return ((DF_TY_FUNC*)typ)->arity;
}   
DF_TYPE_PTR DF_TY_strArgs(DF_TYPE_PTR typ)
{//Note typ must refer to funcAndArgs field
    return (DF_TYPE_PTR)(((MEM_PTR)typ) + DF_TY_ATOMICSIZE);
}

/* TYPE CONSTRUCTION */
void DF_TY_copyAtomic(MEM_PTR sou, MEM_PTR des)
{
    *((DF_TYPE_PTR)des) = *((DF_TYPE_PTR)sou);
}
void DF_TY_mkSort(MEM_PTR loc, MEM_KSTTABIND ind)
{
    ((DF_TY_SORT*)loc)->tag = DF_TY_TAG_SORT;
    ((DF_TY_SORT*)loc)->kindTabIndex = ind;
}
void DF_TY_mkRef(MEM_PTR loc, DF_TYPE_PTR target)
{
    ((DF_TY_REF*)loc)->tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)loc)->target = target;
}
void DF_TY_mkFreeVar(MEM_PTR loc)
{
    ((DF_TY_REF*)loc)->tag = DF_TY_TAG_REF;
    ((DF_TY_REF*)loc)->target = (DF_TYPE_PTR)loc;
}
void DF_TY_mkSkelVar(MEM_PTR loc, MEM_SKELIND offset)
{
    ((DF_TY_SKVAR*)loc)->tag = DF_TY_TAG_SKVAR;
    ((DF_TY_SKVAR*)loc)->offset = offset;
}
void DF_TY_mkArrow(MEM_PTR loc, DF_TYPE_PTR args)
{
    ((DF_TY_ARROW*)loc)->tag = DF_TY_TAG_ARROW;
    ((DF_TY_ARROW*)loc)->args = args;
}
void DF_TY_mkStr(MEM_PTR loc, DF_TYPE_PTR funcAndArgs)
{
    ((DF_TY_STR*)loc)->tag = DF_TY_TAG_STR;
    ((DF_TY_STR*)loc)->funcAndArgs = (DF_TY_FUNC*)funcAndArgs;
}
void DF_TY_mkStrFunc(MEM_PTR loc, MEM_KSTTABIND ind, MEM_TY_ARITY n)
{
    ((DF_TY_FUNC*)loc)->kindTabIndex = ind;
    ((DF_TY_FUNC*)loc)->arity = n;
}

/********************************************************************/
/*                                                                  */
/*                         TERM REPRESENTATION                      */
/*                                                                  */
/********************************************************************/

typedef struct                  //logic variables          
{
    DF_TM_TAG       tag;
    MEM_UNIVIND     univCount;
} DF_TM_VAR;

typedef struct                  //de Bruijn indices
{
    DF_TM_TAG       tag;
    MEM_EMBEDLEV    index;
} DF_TM_BV;

typedef struct {                //name and universe count field for constants
    MEM_UNIVIND     univCount;        
    MEM_CSTTABIND   symTabIndex; 
} NAMEANDUC;

typedef struct {                //constant without type association
    DF_TM_TAG       tag; 
    BOOLEAN         withType;        
    union {
        unsigned int    value;
        NAMEANDUC       nameAndUC;
    } data;
} DF_TM_CONST;

typedef struct {                //constant with type association
    DF_TM_TAG       tag; 
    BOOLEAN         withType;        
    union {
        unsigned int    value;
        NAMEANDUC       nameAndUC;
    } data;
    DF_TYPE_PTR     typeEnv;
} DF_TM_TCONST;

typedef struct                  //integers
{
    DF_TM_TAG        tag;     
    long int         value;
} DF_TM_INT;

typedef struct                  //floats
{
    DF_TM_TAG        tag;     
    float            value;
} DF_TM_FLOAT;

typedef struct                  //string
{
    DF_TM_TAG        tag;
    char             *charList;
} DF_TM_STR;

typedef struct                  //stream
{
    DF_TM_TAG        tag;     
    MEM_STREAMTABIND index;
} DF_TM_STREAM;

typedef struct                  //empty list
{
    DF_TM_TAG        tag;     
} DF_TM_NIL;

typedef struct                  //reference
{
    DF_TM_TAG        tag;     
    DF_TERM_PTR      target;
} DF_TM_REF;

typedef struct                  //list cons
{
    DF_TM_TAG        tag;     
    DF_TERM_PTR      args;
} DF_TM_CONS;

typedef struct                  //abstractions
{
    DF_TM_TAG        tag;     
    MEM_EMBEDLEV     embedLevel; 
    DF_TERM_PTR      body;
} DF_TM_LAM;

typedef struct                  //applications
{
    DF_TM_TAG        tag;     
    MEM_ARITY        arity;
    DF_TERM_PTR      functor;
    DF_TERM_PTR      args;
} DF_TM_APP;

typedef struct                  //suspensions
{
    DF_TM_TAG         tag;   
    MEM_EMBEDLEV      ol;
    MEM_EMBEDLEV      nl;
    DF_TERM_PTR       termSkel;
    DF_ENV_PTR        envList;
} DF_TM_SUSP;


//environment items
typedef struct                  //dummy environment item
{
    BOOLEAN           isDummy;
    MEM_EMBEDLEV      embedLevel;
    DF_ENV_PTR        rest;
} DF_ENV_DUMMY;

typedef struct                  //pair environment item
{
    BOOLEAN          isDummy;
    MEM_EMBEDLEV     embedLevel;
    DF_ENV_PTR       rest;
    DF_TERM_PTR      term;
} DF_ENV_PAIR;


/******************************************************************/
/*                      Interface functions                       */
/******************************************************************/

/* DEREFERENCE      */
DF_TERM_PTR DF_termDeref(DF_TERM_PTR tp)
{
    while (DF_isRef(tp)) tp = ((DF_TM_REF*)tp)->target;
    return tp;
}

/* TERM RECOGNITION */
//note ref is neither atomic nor complex
BOOLEAN DF_isAtomic(DF_TERM_PTR tp)  { return (tp -> tag < DF_TM_TAG_REF);    }
BOOLEAN DF_isNAtomic(DF_TERM_PTR tp) { return (tp -> tag > DF_TM_TAG_REF);    }
BOOLEAN DF_isFV(DF_TERM_PTR tp)      { return (tp -> tag == DF_TM_TAG_VAR);   }
BOOLEAN DF_isConst(DF_TERM_PTR tp)   { return (tp -> tag == DF_TM_TAG_CONST); }
/*assume the tp is known to be a constant */
BOOLEAN DF_isTConst(DF_TERM_PTR tp)  { return ((DF_TM_CONST*)tp) -> withType; }
BOOLEAN DF_isInt(DF_TERM_PTR tp)     { return (tp -> tag == DF_TM_TAG_INT);   }
BOOLEAN DF_isFloat(DF_TERM_PTR tp)   { return (tp -> tag == DF_TM_TAG_FLOAT); }
BOOLEAN DF_isNil(DF_TERM_PTR tp)     { return (tp -> tag == DF_TM_TAG_NIL);   }
BOOLEAN DF_isStr(DF_TERM_PTR tp)     { return (tp -> tag == DF_TM_TAG_STR);   }
BOOLEAN DF_isBV(DF_TERM_PTR tp)      { return (tp -> tag == DF_TM_TAG_BVAR);  }
BOOLEAN DF_isStream(DF_TERM_PTR tp)  { return (tp -> tag == DF_TM_TAG_STREAM);}
BOOLEAN DF_isRef(DF_TERM_PTR tp)     { return (tp -> tag == DF_TM_TAG_REF);   }
BOOLEAN DF_isCons(DF_TERM_PTR tp)    { return (tp -> tag == DF_TM_TAG_CONS);  }
BOOLEAN DF_isLam(DF_TERM_PTR tp)     { return (tp -> tag == DF_TM_TAG_LAM);   }
BOOLEAN DF_isApp(DF_TERM_PTR tp)     { return (tp-> tag == DF_TM_TAG_APP);    }
BOOLEAN DF_isSusp(DF_TERM_PTR tp)    { return (tp-> tag == DF_TM_TAG_SUSP);   }
BOOLEAN DF_isEmpEnv(DF_ENV_PTR ep)   { return (ep == DF_EMPTYENV);            }
BOOLEAN DF_isDummyEnv(DF_ENV_PTR ep) { return ep -> isDummy;                  }


/* TERM DECOMPOSITION */
DF_TM_TAG DF_tag(DF_TERM_PTR tp)              // tag 
{
    return tp -> tag;
}
//unbound variables
MEM_UNIVIND DF_FVUnivCount(DF_TERM_PTR tp)    //universe count
{
    return ((DF_TM_VAR*)tp)->univCount;
}
//constant (w/oc type associations)
MEM_UNIVIND DF_constUnivCount(DF_TERM_PTR tp) //universe count
{
    return ((DF_TM_CONST*)tp)->data.nameAndUC.univCount;
}
MEM_CSTTABIND DF_constTabIndex(DF_TERM_PTR tp)
{
    return ((DF_TM_CONST*)tp)->data.nameAndUC.symTabIndex;
}
//constants with type associations
DF_TYPE_PTR DF_TConstType(DF_TERM_PTR tp)     //type env
{
    return ((DF_TM_TCONST*)tp)->typeEnv;
}
//integer
long DF_intValue(DF_TERM_PTR tp)              //integer value
{
    return ((DF_TM_INT*)tp)->value;
}
long DF_intABS(DF_TERM_PTR tp)                //absolute value
{
    return labs(((DF_TM_INT*)tp)->value);
}
long DF_intNEG(DF_TERM_PTR tp)                //negation
{
    return -(((DF_TM_INT*)tp)->value);
}
//float
float DF_floatValue(DF_TERM_PTR tp)           //float value
{
    return ((DF_TM_FLOAT*)tp)->value;
}
float DF_floatABS(DF_TERM_PTR tp)             //absolute value
{
    return fabs(((DF_TM_FLOAT*)tp)->value);
}
float DF_floatNEG(DF_TERM_PTR tp)             //negation
{
    return -(((DF_TM_FLOAT*)tp)->value);
}
//string
char* DF_strValue(DF_TERM_PTR tp)             //string value
{
    return ((DF_TM_STR*)tp)->charList;
}
int   DF_StrLength(DF_TERM_PTR tp)            //string length
{
    return strlen(((DF_TM_STR*)tp)->charList);
}
//stream
MEM_STREAMTABIND DF_streamTabIndex(DF_TERM_PTR tp)  //stream table index
{
    return ((DF_TM_STREAM*)tp)->index;
}
//de Bruijn index
MEM_EMBEDLEV DF_BVIndex(DF_TERM_PTR tp)       //de Bruijn index
{
    return ((DF_TM_BV*)tp)->index;
}
//reference
DF_TERM_PTR DF_refTarget(DF_TERM_PTR tp)      //target
{ 
    return ((DF_TM_REF*)tp)->target;
}
//list cons
DF_TERM_PTR DF_consArgs(DF_TERM_PTR tp)       //arg vector
{
    return ((DF_TM_CONS*)tp)->args;
}
//abstraction
MEM_EMBEDLEV DF_lamEmbedLev(DF_TERM_PTR tp)   //embedding level
{
    return ((DF_TM_LAM*)tp)->embedLevel;
}
DF_TERM_PTR DF_lamBody(DF_TERM_PTR tp)        //abstraction body
{
    return ((DF_TM_LAM*)tp)->body;
}
//application
MEM_ARITY DF_appArity(DF_TERM_PTR tp)         //arity
{
    return ((DF_TM_APP*)tp)->arity;
}
DF_TERM_PTR DF_appFunc(DF_TERM_PTR tp)        //functor
{
    return ((DF_TM_APP*)tp)->functor;
}
DF_TERM_PTR DF_appArgs(DF_TERM_PTR tp)        //arg vector
{
    return ((DF_TM_APP*)tp)->args;
}
//suspension
MEM_EMBEDLEV DF_suspOL(DF_TERM_PTR tp)       //ol
{
    return ((DF_TM_SUSP*)tp)->ol;
}
MEM_EMBEDLEV DF_suspNL(DF_TERM_PTR tp)       //nl
{
    return ((DF_TM_SUSP*)tp)->nl;
}
DF_TERM_PTR DF_suspTermSkel(DF_TERM_PTR tp)  //term skeleton
{
    return ((DF_TM_SUSP*)tp)->termSkel;
}
DF_ENV_PTR DF_suspEnv(DF_TERM_PTR tp)        //environment list
{
    return ((DF_TM_SUSP*)tp)->envList;
}

//environment item (dummy/pair)
DF_ENV_PTR DF_envListRest(DF_ENV_PTR ep)     //next env item
{
    return ep->rest;
}
DF_ENV_PTR DF_envListNth(DF_ENV_PTR ep, int n) //nth item 
{
    int i; 
    for (i=n; (i!=1); i--) ep = ep -> rest;
    return ep;
}
MEM_EMBEDLEV DF_envIndex(DF_ENV_PTR ep)      //l in @l or (t,l) 
{
    return ep -> embedLevel;
}
//pair environment item
DF_TERM_PTR DF_envPairTerm(DF_ENV_PTR ep)    //t in (t,l)
{
    return ((DF_ENV_PAIR*)ep) -> term;
}

/* TERM CONSTRUCTION */
void DF_copyAtomic(MEM_PTR sou, MEM_PTR des)                  //copy atomic 
{
    *((DF_TERM_PTR)des) = *((DF_TERM_PTR)sou);
}
void DF_mkVar(MEM_PTR loc, MEM_UNIVIND uc)                    //unbound variable
{
    ((DF_TM_VAR*)loc) -> tag = DF_TM_TAG_VAR;
    ((DF_TM_VAR*)loc) -> univCount = uc;
}
void DF_mkBV(MEM_PTR loc, MEM_EMBEDLEV ind)                   //de Bruijn index
{
    ((DF_TM_BV*)loc) -> tag = DF_TM_TAG_BVAR;
    ((DF_TM_BV*)loc) -> index = ind;
}
void DF_mkConst(MEM_PTR loc, MEM_UNIVIND uc, MEM_CSTTABIND ind) //const 
{
    ((DF_TM_CONST*)loc) -> tag = DF_TM_TAG_CONST;
    ((DF_TM_CONST*)loc) -> withType = FALSE;
    (((DF_TM_CONST*)loc) -> data).nameAndUC.univCount = uc;
    (((DF_TM_CONST*)loc) -> data).nameAndUC.symTabIndex = ind;
}
void DF_mkTConst(MEM_PTR loc, MEM_UNIVIND uc, MEM_CSTTABIND ind, 
                 DF_TYPE_PTR typeEnv)        //const with type association
{
    ((DF_TM_TCONST*)loc) -> tag = DF_TM_TAG_CONST;
    ((DF_TM_TCONST*)loc) -> withType = TRUE;
    (((DF_TM_TCONST*)loc) -> data).nameAndUC.univCount = uc;
    (((DF_TM_TCONST*)loc) -> data).nameAndUC.symTabIndex = ind;
    ((DF_TM_TCONST*)loc) -> typeEnv = typeEnv;
}
void DF_mkInt(MEM_PTR loc, long value)                        //int
{
    ((DF_TM_INT*)loc) -> tag = DF_TM_TAG_INT;
    ((DF_TM_INT*)loc) -> value = value;
}
void DF_mkFloat(MEM_PTR loc, float value)                     //float
{
    ((DF_TM_FLOAT*)loc) -> tag = DF_TM_TAG_FLOAT;
    ((DF_TM_FLOAT*)loc) -> value = value;
}
void DF_mkStr(MEM_PTR loc, char *value)                       //string
{
    ((DF_TM_STR*)loc) -> tag = DF_TM_TAG_STR;
    ((DF_TM_STR*)loc) -> charList = value;
}
void DF_mkStream(MEM_PTR loc, MEM_STREAMTABIND ind)           //stream
{
    ((DF_TM_STREAM*)loc) -> tag = DF_TM_TAG_STREAM;
    ((DF_TM_STREAM*)loc) -> index = ind;
}
void DF_mkNil(MEM_PTR loc)                                    //nil
{
    ((DF_TM_NIL*)loc) -> tag = DF_TM_TAG_NIL;
}
void DF_mkRef(MEM_PTR loc, DF_TERM_PTR target)                //reference
{
    ((DF_TM_REF*)loc) -> tag = DF_TM_TAG_REF;
    ((DF_TM_REF*)loc) -> target = target;
}
void DF_mkCons(MEM_PTR loc, DF_TERM_PTR args)                 //cons
{
    ((DF_TM_CONS*)loc) -> tag = DF_TM_TAG_CONS;
    ((DF_TM_CONS*)loc) -> args = args;
}
void DF_mkLam(MEM_PTR loc, MEM_EMBEDLEV n, DF_TERM_PTR body)  //abstraction
{
    ((DF_TM_LAM*)loc) -> tag = DF_TM_TAG_LAM;
    ((DF_TM_LAM*)loc) -> embedLevel = n;
    ((DF_TM_LAM*)loc) -> body = body;
}
void DF_mkApp(MEM_PTR loc, MEM_ARITY n, DF_TERM_PTR func, DF_TERM_PTR args) 
{                                                             //application
    ((DF_TM_APP*)loc) -> tag = DF_TM_TAG_APP;
    ((DF_TM_APP*)loc) -> arity = n;
    ((DF_TM_APP*)loc) -> functor = func;
    ((DF_TM_APP*)loc) -> args = args;
}
void DF_mkSusp(MEM_PTR loc, MEM_EMBEDLEV ol, MEM_EMBEDLEV nl, DF_TERM_PTR tp,
               DF_ENV_PTR env)                                //suspension
{
    ((DF_TM_SUSP*)loc) -> tag = DF_TM_TAG_SUSP;
    ((DF_TM_SUSP*)loc) -> ol = ol;
    ((DF_TM_SUSP*)loc) -> nl = nl;
    ((DF_TM_SUSP*)loc) -> termSkel = tp;
    ((DF_TM_SUSP*)loc) -> envList = env;
}

void DF_mkDummyEnv(MEM_PTR loc, MEM_EMBEDLEV l, DF_ENV_PTR rest) //@l env item
{
    ((DF_ENV_DUMMY*)loc) -> isDummy = TRUE;
    ((DF_ENV_DUMMY*)loc) -> embedLevel = l;
    ((DF_ENV_DUMMY*)loc) -> rest = rest;
}
void DF_mkPairEnv(MEM_PTR loc, MEM_EMBEDLEV l, DF_TERM_PTR t, DF_ENV_PTR rest)
{                                                            // (t, l) env item
    ((DF_ENV_PAIR*)loc) -> isDummy = FALSE;
    ((DF_ENV_PAIR*)loc) -> embedLevel = l;
    ((DF_ENV_PAIR*)loc) -> term = t;
    ((DF_ENV_PAIR*)loc) -> rest = rest;
}


/* SPECIAL CONSTANTS  */

BOOLEAN DF_sameStr(DF_TERM_PTR str1, DF_TERM_PTR str2)      //same string?
{
    if (str1 == str2) return TRUE;
    return (strcmp(((DF_TM_STR*)str1)->charList, (((DF_TM_STR*)str2)->charList))
            == 0);
}

#endif  //DATAFORMAT_C
