/****************************************************************************/
/*                                                                          */
/* File dataformat.h.                                                       */
/* The header file identifies the low-level representation of data objects  */
/* that are manipulated by the machine, through various structure types.    */ 
/****************************************************************************/
#ifndef DATAFORMAT_H
#define DATAFORMAT_H

#include <limits.h>
#include <stdlib.h>
#include "mctypes.h"
#include "../system/memory.h"

/********************************************************************/
/*                                                                  */
/*                         TYPE REPRESENTATION                      */
/*                                                                  */
/********************************************************************/

/********************************************************************/
/* Only generic types are visible from outside.                     */
/* The "public" information for each specific type category is their*/
/* sizes. Their structure declarations are hidden in dataformat.c.  */
/* Construction, recognization and decomposition of types should be */
/* performed through interface functions with declarations present  */
/* in this file.                                                    */
/********************************************************************/

//type category tag
typedef BYTE DF_TY_TAG;

//type categories
enum DF_TypeCategory
{
    DF_TY_TAG_SORT,  //sort
    DF_TY_TAG_REF,   //reference
    DF_TY_TAG_SKVAR, //skeleton variable
    DF_TY_TAG_ARROW, //type arrow
    DF_TY_TAG_STR    //type structure
};

//generic type (head) for every category
typedef struct               
{
    DF_TY_TAG       tag;     /* the common field for every type (head); can 
                                be any one of enum TypeCategory.
                                rely on struct alignment */ 
    void            *dummy;  /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TYPE;

typedef DF_TYPE *DF_TYPE_PTR; //type pointer

//sizes of different type items
#define DF_TY_ATOMICSIZE    2    //atomic size

//attributes of special type constructors
#define DF_TY_ARROWARITY    2    //arity of type arrow

/******************************************************************/
/*                      Interface functions                       */
/******************************************************************/

/* TYPE DEREFERENCE */
DF_TYPE_PTR DF_typeDeref(DF_TYPE_PTR);

/* TYPE RECOGNITION */
BOOLEAN DF_TY_isSort(DF_TYPE_PTR);     // is sort?
BOOLEAN DF_TY_isRef(DF_TYPE_PTR);      // is reference? (including free var)
BOOLEAN DF_TY_isFreeVar(DF_TYPE_PTR);  // is free var?
BOOLEAN DF_TY_isSkelVar(DF_TYPE_PTR);  // is skeleton var?
BOOLEAN DF_TY_isArrow(DF_TYPE_PTR);    // is type arrow?
BOOLEAN DF_TY_isStr(DF_TYPE_PTR);      // is type structure?

/* TYPE DECOMPOSITION */
DF_TY_TAG DF_TY_tag(DF_TYPE_PTR);                      //generic type
MEM_KSTTABIND DF_TY_kindTabIndex(DF_TYPE_PTR);         //sorts
MEM_SKELIND DF_TY_skelVarIndex(DF_TYPE_PTR);           //skel var
DF_TYPE_PTR DF_TY_refTarget(DF_TYPE_PTR);              //reference
DF_TYPE_PTR DF_TY_arrowArgs(DF_TYPE_PTR);              //arrows
DF_TYPE_PTR DF_TY_strFuncAndArgs(DF_TYPE_PTR);         //structures
MEM_KSTTABIND DF_TY_strFuncInd(DF_TYPE_PTR);           
MEM_TY_ARITY DF_TY_strFuncArity(DF_TYPE_PTR); 
DF_TYPE_PTR DF_TY_strArgs(DF_TYPE_PTR);

/* TYPE CONSTRUCTION */
void DF_TY_copyAtomic(MEM_PTR sou, MEM_PTR des);
void DF_TY_mkSort(MEM_PTR loc, MEM_KSTTABIND ind);
void DF_TY_mkRef(MEM_PTR loc, DF_TYPE_PTR target);
void DF_TY_mkFreeVar(MEM_PTR loc);
void DF_TY_mkSkelVar(MEM_PTR loc, MEM_SKELIND offset);
void DF_TY_mkArrow(MEM_PTR loc, DF_TYPE_PTR args);
void DF_TY_mkStr(MEM_PTR loc, DF_TYPE_PTR funcAndArgs);
void DF_TY_mkStrFunc(MEM_PTR loc, MEM_KSTTABIND ind, MEM_TY_ARITY n);


/********************************************************************/
/*                                                                  */
/*                         TERM REPRESENTATION                      */
/*                                                                  */
/********************************************************************/

/********************************************************************/
/* Only generic terms (environment items) are visible from outside. */
/* The "public" information for each specific term category is their*/
/* sizes. Their structure declarations are hidden in dataformat.c.  */
/* Construction, recognization and decomposition of terms should be */
/* performed through interface functions with declarations present  */
/* in this file.                                                    */
/********************************************************************/

//term category tag
typedef BYTE DF_TM_TAG;
 
//term categories
enum DF_TermCategory 
{ 
    DF_TM_TAG_VAR,            // existential variables
    DF_TM_TAG_CONST,          // constants 
    DF_TM_TAG_INT,            // integers
    DF_TM_TAG_FLOAT,          // floats
    DF_TM_TAG_NIL,            // empty lists
    DF_TM_TAG_STR,            // strings
    DF_TM_TAG_STREAM,         // streams
    DF_TM_TAG_BVAR,           // lambda bound variables (de Bruijn index)
                              // -- atoms above
    DF_TM_TAG_REF,            // references
                              // -- complex terms below
    DF_TM_TAG_CONS,           // list constructors
    DF_TM_TAG_LAM,            // abstractions
    DF_TM_TAG_APP,            // applications
    DF_TM_TAG_SUSP            // suspensions
};


// a generic term (head) for every category
typedef struct               
{
    DF_TM_TAG    tag;        /* the common field for every term (head); can 
                                be any one of enum TermCategory.
                                rely on struct alignment */ 
    void         *dummy;     /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TERM;

typedef DF_TERM *DF_TERM_PTR; //term pointer


//sizes of different term items
#define DF_TM_ATOMICSIZE  2       // atomic size
#define DF_TM_TCONSTSIZE  3       // type associated constant   
#define DF_TM_APPSIZE     3       // application head
#define DF_TM_LAMSIZE     2       // abstraction
#define DF_TM_CONSSIZE    2       // cons 
#define DF_TM_SUSPSIZE    4       // suspension 

// attributes of some special constants 
#define  DF_CONSARITY  2          //arity of cons

//a generic environment item in suspension
typedef struct DF_env
{
    BOOLEAN          isDummy;
    MEM_EMBEDLEV     embedLevel;
    struct DF_env    *rest;    //the tail of the list
} DF_ENV;

typedef DF_ENV *DF_ENV_PTR;

// empty environment list 
#define DF_EMPTYENV NULL

//sizes of different environment items
#define DF_ENV_DUMMYSIZE  2      // dummy environment item
#define DF_ENV_PAIRSIZE   3      // pair environment item

/******************************************************************/
/*                      Interface functions                       */
/******************************************************************/

/* DEREFERENCE      */
DF_TERM_PTR DF_termDeref(DF_TERM_PTR); // term dereference

/* TERM RECOGNITION */
BOOLEAN DF_isAtomic(DF_TERM_PTR); //note ref is neither atomic nor complex
BOOLEAN DF_isNAtomic(DF_TERM_PTR);                                    
BOOLEAN DF_isFV(DF_TERM_PTR);     // is unbound variable?
BOOLEAN DF_isConst(DF_TERM_PTR);  // is constant (typed and untyped)?
BOOLEAN DF_isTConst(DF_TERM_PTR); // is a type associated constant? 
                                  // Note we assume the arg is known to be const
BOOLEAN DF_isInt(DF_TERM_PTR);    // is integer?
BOOLEAN DF_isFloat(DF_TERM_PTR);  // is float?
BOOLEAN DF_isNil(DF_TERM_PTR);    // is list nil?
BOOLEAN DF_isStr(DF_TERM_PTR);    // is string?
BOOLEAN DF_isBV(DF_TERM_PTR);     // is de Bruijn index?
BOOLEAN DF_isStream(DF_TERM_PTR); // is stream?
BOOLEAN DF_isRef(DF_TERM_PTR);    // is reference?
BOOLEAN DF_isCons(DF_TERM_PTR);   // is list cons?
BOOLEAN DF_isLam(DF_TERM_PTR);    // is abstraction?
BOOLEAN DF_isApp(DF_TERM_PTR);    // is application?
BOOLEAN DF_isSusp(DF_TERM_PTR);   // is suspension?

BOOLEAN DF_isEmpEnv(DF_ENV_PTR);  // is empty environment?
BOOLEAN DF_isDummyEnv(DF_ENV_PTR);// is dummy environment item?

/* TERM DECOMPOSITION */
//generic term
DF_TM_TAG DF_tag(DF_TERM_PTR);               // term category tag
//unbound variable
MEM_UNIVIND DF_FVUnivCount(DF_TERM_PTR);     // universe count
//constants (w/oc type associations)
MEM_UNIVIND DF_constUnivCount(DF_TERM_PTR);  // universe index
MEM_CSTTABIND DF_constTabIndex(DF_TERM_PTR); // symbol table index
//constants with type associations
DF_TYPE_PTR DF_TConstType(DF_TERM_PTR);      // type environment
//integer
long DF_intValue(DF_TERM_PTR);               // integer value (long)
long DF_intABS(DF_TERM_PTR);                 // absolute value
long DF_intNEG(DF_TERM_PTR);                 // negation
//float
float DF_floatValue(DF_TERM_PTR);            // float value
float DF_floatABS(DF_TERM_PTR);              // absolute value
float DF_floatNEG(DF_TERM_PTR);              // negation
//string
char* DF_strValue(DF_TERM_PTR);              // string value
int   DF_strLength(DF_TERM_PTR);             // string length
//stream
MEM_STREAMTABIND DF_streamTabIndex(DF_TERM_PTR);  // stream table index
//de Bruijn indices
MEM_EMBEDLEV DF_BVIndex(DF_TERM_PTR);         // de Bruijn index
//reference
DF_TERM_PTR DF_refTarget(DF_TERM_PTR);       // target
//list cons
DF_TERM_PTR DF_consArgs(DF_TERM_PTR);        // arg vector
//abstractions
MEM_EMBEDLEV DF_lamEmbedLev(DF_TERM_PTR);    // embedding level
DF_TERM_PTR DF_lamBody(DF_TERM_PTR);         // lambda body
//application
MEM_ARITY DF_appArity(DF_TERM_PTR);          // arity
DF_TERM_PTR DF_appFunc(DF_TERM_PTR);         // functor
DF_TERM_PTR DF_appArgs(DF_TERM_PTR);         // arg vector
//suspension
MEM_EMBEDLEV DF_suspOL(DF_TERM_PTR);         // ol
MEM_EMBEDLEV DF_suspNL(DF_TERM_PTR);         // nl
DF_TERM_PTR DF_suspTermSkel(DF_TERM_PTR);    // term skel
DF_ENV_PTR  DF_suspEnv(DF_TERM_PTR);         // environment list

//environment item (dummy/pair)
DF_ENV_PTR DF_envListRest(DF_ENV_PTR);       // next env item
DF_ENV_PTR DF_envListNth(DF_ENV_PTR, int);   // the nth item 
MEM_EMBEDLEV DF_envIndex(DF_ENV_PTR);        // l in @l or (t,l)
//pair environment item 
DF_TERM_PTR DF_envPairTerm(DF_ENV_PTR);      // t in (t,l)


/* TERM CONSTRUCTION */
void DF_copyAtomic(MEM_PTR sou, MEM_PTR des);    //copy atomic 
void DF_mkVar(MEM_PTR loc, MEM_UNIVIND uc);      //unbound variable
void DF_mkBV(MEM_PTR loc, MEM_EMBEDLEV ind);     //de Bruijn index
void DF_mkConst(MEM_PTR loc, MEM_UNIVIND uc, MEM_CSTTABIND ind); //const 
void DF_mkTConst(MEM_PTR loc, MEM_UNIVIND uc, MEM_CSTTABIND ind, 
                 DF_TYPE_PTR typeEnv);           //const with type association
void DF_mkInt(MEM_PTR loc, long value);          //int
void DF_mkFloat(MEM_PTR loc, float value);       //float
void DF_mkStr(MEM_PTR loc, char *value);         //string
void DF_mkStream(MEM_PTR loc, MEM_STREAMTABIND ind);  //stream
void DF_mkNil(MEM_PTR loc);                      //nil
void DF_mkRef(MEM_PTR loc, DF_TERM_PTR target);  //reference
void DF_mkCons(MEM_PTR loc, DF_TERM_PTR args);   //cons
void DF_mkLam(MEM_PTR loc, MEM_EMBEDLEV n, DF_TERM_PTR body);    //abstraction
void DF_mkApp(MEM_PTR loc, MEM_ARITY n, DF_TERM_PTR func, DF_TERM_PTR args); 
                                                 //application
void DF_mkSusp(MEM_PTR loc, MEM_EMBEDLEV ol, MEM_EMBEDLEV nl, DF_TERM_PTR tp,
               DF_ENV_PTR env);                  //suspension
void DF_mkDummyEnv(MEM_PTR loc, MEM_EMBEDLEV l, DF_ENV_PTR rest); //@l env item
void DF_mkPairEnv(MEM_PTR loc, MEM_EMBEDLEV l, DF_TERM_PTR t, DF_ENV_PTR rest);
                                                 // (t, l) env item

/* SPECIAL CONSTANTS */
BOOLEAN DF_sameStr(DF_TERM_PTR str1, DF_TERM_PTR str2);         //same string?

#endif  //DATAFORMAT_H


