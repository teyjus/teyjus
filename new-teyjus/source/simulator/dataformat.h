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

/********************************************************************/
/*                                                                  */
/*          Type Representation                                     */
/*                                                                  */
/********************************************************************/

/*type declarations for kind table index and type structure arity   */
typedef unsigned short int DF_TY_TABIND;  //kind table index
typedef unsigned short int DF_TY_ARITY;   //type structure arity


typedef BYTE  DF_TY_TAG;

//type categories
enum DF_TypeCategory
{
    DF_TY_TAG_SORT,  //sort
    DF_TY_TAG_REF,   //reference
    DF_TY_TAG_ARROW, //type arrow
    DF_TY_TAG_STR    //type structure
};


//generic type (head) for every category
typedef struct               
{
    DF_TY_TAG       tag;    /* the common field for every type (head); can 
                               be any one of enum TypeCategory.
                                rely on struct alignment */ 
    void            *dummy;  /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TYPE;

typedef DF_TYPE *DF_TYPE_PTR; //type pointer

/***********************************************/
/* interface functions for type recognization  */
/***********************************************/

BOOLEAN DF_TY_IsSort(DF_TYPE_PTR);  // is sort?

BOOLEAN DF_TY_IsRef(DF_TYPE_PTR);   // is reference? (including free var)

BOOLEAN DF_TY_IsFreeVar(DF_TYPE_PTR);  // is free var?

BOOLEAN DF_TY_IsArrow(DF_TYPE_PTR);   // is type arrow?

BOOLEAN DF_TY_IsStr(DF_TYPE_PTR);     // is type structure?

/*********************************************/
/*interface functions for type decomposition */
/*********************************************/
//generic type
DF_TY_TAG DF_TY_Tag(DF_TYPE_PTR);   // extracting tag

//sorts
DF_TY_TABIND DF_TY_KTableIndex(DF_TYPE_PTR); // extracting kind table index

//reference
DF_TYPE_PTR DF_TY_RefTarget(DF_TYPE_PTR); // extracting target

//arrows
DF_TYPE_PTR DF_TY_ArrowArgs(DF_TYPE_PTR); //extracting addr of args

//structures
//extracting kind table index of functor
DF_TY_TABIND DF_TY_StrFunc(DF_TYPE_PTR); 
DF_TY_ARITY DF_TY_StrArity(DF_TYPE_PTR); //extracting arity of functor
DF_TYPE_PTR DF_TY_StrArgs(DF_TYPE_PTR); //extracting address of arg vector
//extracting address of func which is immediately followed by arg vector 
DF_TYPE_PTR DF_TY_StrFuncAndArgs(DF_TYPE_PTR);


/*********************************************/
/* interface functions for type cobstruction */
/*********************************************/
//sort
void DF_TY_MkSort_(DF_TYPE_PTR, DF_TY_TABIND);
DF_TYPE_PTR DF_TY_MkSort(DF_TYPE_PTR, DF_TY_TABIND);

//reference
void DF_TY_MkRef_(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_TY_MkRef(DF_TYPE_PTR, DF_TYPE_PTR);

//free variable
void DF_TY_MkFreeVar_(DF_TYPE_PTR);
DF_TYPE_PTR DF_TY_MkFreeVar(DF_TYPE_PTR);

//arrows
void DF_TY_MkArrow_(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_TY_MkArrow(DF_TYPE_PTR, DF_TYPE_PTR);

//structures
void DF_TY_MkStr_(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_TY_MkStr(DF_TYPE_PTR, DF_TYPE_PTR);

//functor can only be created on the heap
DF_TYPE_PTR DF_TY_MkFunc(DF_TYPE_PTR, DF_TY_TABIND, DF_TY_ARITY);

/********************/
/* type dereference */
/********************/
DF_TYPE_PTR DF_TY_Deref(DF_TYPE_PTR);


/****************************************************************************
 *                                                                          *
 *                         TERM REPRESENTATION                              *
 *                                                                          *
 ****************************************************************************/
/* Only generic terms are visible to other functions.
   The structures of specific terms are hidden in dataformat.c.
   The construction, recognization and decomposition (and copy) 
   of term structures should be performed through the interface functions.
*/


/* type declarations for universe counter, (symbol) table index, 
   abstraction level and application arity fields for various terms and types*/
typedef unsigned short int DF_UNIVIND;     //universe index
typedef unsigned short int DF_EMBEDLEV;    //abstraction context
typedef unsigned short int DF_ARITY;       //arity

typedef unsigned int DF_TABIND;            //symbol table index


typedef BYTE DF_TAG;         // term category tag

//term categories
enum DF_TermCategory 
{ 
    DF_TM_TAG_VAR,            // existential variables
    DF_TM_TAG_CONST,          // constants 
    DF_TM_TAG_INT,            // integers
    DF_TM_TAG_FLOAT,          // floats
    DF_TM_TAG_NIL,            // empty lists
    DF_TM_TAG_STR,            // strings
    DF_TM_TAG_BVAR,           // lambda bound variables (de Bruijn index)
    DF_TM_TAG_STREAM,         // streams
    DF_TM_TAG_REF,            // references 
                             // all categories above are atomic terms 
    DF_TM_TAG_CONS,           // list constructors
    DF_TM_TAG_LAM,            // abstractions
    DF_TM_TAG_APP,            // applications
    DF_TM_TAG_SUSP            // suspensions
};

// a generic term (head) for every category
typedef struct               
{
    DF_TAG        tag;       /* the common field for every term (head); can 
                                be any one of enum TermCategory.
                                rely on struct alignment */ 
    void         *dummy;     /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TERM;

typedef DF_TERM *DF_TERM_PTR; //term pointer



// environment items (list) in suspension

/* The common fields of ENV and DUMMYENV have to be put in same places with 
   respect to the structures: rely on struct alignment. */
typedef struct DF_env               // pair environment item
{
    struct DF_env      *rest;
    BOOLEAN            isDummy;   // isDummy = 0
    DF_EMBEDLEV        embedLevel;
    DF_TERM_PTR        term;
} DF_ENV;


typedef struct           // dummy environment item
{
    DF_ENV       *rest;
    BOOLEAN      isDummy;   // isDummy = 1
    DF_EMBEDLEV  embedLevel;
    
} DF_DUMMYENV;

typedef DF_ENV *DF_ENV_PTR;

/*********************************************/
/*interface functions for term recognition   */
/*********************************************/

BOOLEAN DF_IsAtomic(DF_TERM_PTR);      

BOOLEAN DF_IsNAtomic(DF_TERM_PTR);  /* whether a term is atomic: ref is not
                                    included in atomic terms nor non-atomic
                                    ones */
BOOLEAN DF_IsFV(DF_TERM_PTR);     // is variable?

BOOLEAN DF_IsConst(DF_TERM_PTR);   // is constant (typed and untyped)?

BOOLEAN DF_IsInt(DF_TERM_PTR);     // is integer?

BOOLEAN DF_IsFloat(DF_TERM_PTR);   // is float?

BOOLEAN DF_IsNil(DF_TERM_PTR);     // is empty list?

BOOLEAN DF_IsStr(DF_TERM_PTR);     // is string?

BOOLEAN DF_IsBV(DF_TERM_PTR);    // is lambda bound variable?

BOOLEAN DF_IsStream(DF_TERM_PTR);  // is stream?

BOOLEAN DF_IsRef(DF_TERM_PTR);     // is reference?

BOOLEAN DF_IsCons(DF_TERM_PTR);    // is list cons?

BOOLEAN DF_IsLam(DF_TERM_PTR);     // is abstraction?

BOOLEAN DF_IsApp(DF_TERM_PTR);     // is application?

BOOLEAN DF_IsSusp(DF_TERM_PTR);    // is suspension?

//environment item (list)
BOOLEAN DF_EmpEnv(DF_ENV_PTR);     // is empty environment?

BOOLEAN DF_IsDummy(DF_ENV_PTR);    // is dummy environment item?




/*********************************************/
/*interface functions for term decomposition */
/*********************************************/

//generic term
DF_TAG DF_Tag(DF_TERM_PTR);        // extracting tag from a generic term

//constants (typed/untyped) and variable
DF_UNIVIND DF_UnivIndex(DF_TERM_PTR);  // exacting universe index

//constants (typed/untyped)
DF_UNIVIND DF_ConstUnivIndex(DF_TERM_PTR); //extracting universe index
DF_TABIND DF_ConstTabIndex(DF_TERM_PTR);   //extracting symbol table index

//typed constants
DF_TYPE_PTR DF_TConstType(DF_TERM_PTR);    //extracting the address of type env

//integer
long DF_IntValue(DF_TERM_PTR);             //extracting integer value (long)
long DF_IntABS(DF_TERM_PTR);               //absolute value
long DF_IntNEG(DF_TERM_PTR);               //negation

//float
float DF_FloatValue(DF_TERM_PTR);          //extracting float value
float DF_FloatABS(DF_TERM_PTR);            //absolute value
float DF_FloatNEG(DF_TERM_PTR);            //negation

//string
char* DF_StrValue(DF_TERM_PTR);            //extracting string value
int   DF_StrLength(DF_TERM_PTR);           //extracting string length

//stream
DF_TABIND DF_StreamTabIndex(DF_TERM_PTR);  //extracting stream index

//lambda bound variable
DF_EMBEDLEV DF_BVIndex(DF_TERM_PTR);       //extracting de Bruijn index

//reference
DF_TERM_PTR DF_RefTarget(DF_TERM_PTR);     //extracting the address of target

//list cons
DF_TERM_PTR DF_ConsArgs(DF_TERM_PTR);   //extracting the address of args of cons

//abstractions
DF_EMBEDLEV DF_LamEmbedLev(DF_TERM_PTR); //extracting abstraction level
DF_TERM_PTR DF_LamBody(DF_TERM_PTR);     //extracting the address of lambda body

//application
DF_ARITY DF_AppArity(DF_TERM_PTR);     //extracting arity
DF_TERM_PTR DF_AppFunc(DF_TERM_PTR);   //extracting the address of functor
DF_TERM  DF_AppFuncTerm(DF_TERM_PTR);  //extracting the functor
DF_TERM_PTR DF_AppArgs(DF_TERM_PTR);   //extracting the address of arg vector

//suspension
DF_EMBEDLEV DF_SuspOL(DF_TERM_PTR);        //extracting ol
DF_EMBEDLEV DF_SuspNL(DF_TERM_PTR);        //extracting nl
DF_TERM_PTR DF_SuspTermSkel(DF_TERM_PTR);  //extracting the address of term skel
DF_ENV_PTR  DF_SuspEnv(DF_TERM_PTR);       //extracting the environment list


//environment item (dummy/pair)
DF_EMBEDLEV DF_EnvIndex(DF_ENV_PTR);           //extracting l in @l or (t,l)

//pair environment item 
DF_TERM_PTR DF_EnvTerm(DF_ENV_PTR);    //extracting the address of t in (t,l)

//environment item (list) (dummy/pair)
DF_ENV_PTR DF_EnvListRest(DF_ENV_PTR); //extracting the tail of env list

//environment list 
DF_ENV_PTR DF_EnvNth(DF_ENV_PTR, int); //extracting the nth item in the env list


/*********************************************/
/*interface functions for term construction  */
/*********************************************/

//copy atomic terms 
void DF_CopyAtomic_(DF_TERM_PTR, DF_TERM_PTR);
DF_TERM_PTR DF_CopyAtomic(DF_TERM_PTR, DF_TERM_PTR);


//variable
void DF_MkVar_(DF_TERM_PTR, DF_UNIVIND);
DF_TERM_PTR DF_MkVar(DF_TERM_PTR, DF_UNIVIND);

//lambda bound variable
void DF_MkBV_(DF_TERM_PTR, DF_EMBEDLEV);
DF_TERM_PTR DF_MkBV(DF_TERM_PTR, DF_EMBEDLEV);

//untyped constant
void DF_MkConst_(DF_TERM_PTR, DF_UNIVIND, DF_TABIND);
DF_TERM_PTR DF_MkConst(DF_TERM_PTR, DF_UNIVIND, DF_TABIND);

//typed constant
void DF_MkTConst_(DF_TERM_PTR, DF_UNIVIND, DF_TABIND, DF_TYPE_PTR); 
DF_TERM_PTR DF_MkTConst(DF_TERM_PTR, DF_UNIVIND, DF_TABIND, DF_TYPE_PTR);

//integer
void DF_MkInt_(DF_TERM_PTR, long);
DF_TERM_PTR DF_MkInt(DF_TERM_PTR, long);

//float
void DF_MkFloat_(DF_TERM_PTR, float);
DF_TERM_PTR DF_MkFloat(DF_TERM_PTR, float);

//string
void DF_MkStr_(DF_TERM_PTR, char*);
DF_TERM_PTR DF_MkStr(DF_TERM_PTR, char*);

//stream
void DF_MkStream_(DF_TERM_PTR, DF_TABIND);
DF_TERM_PTR DF_MkStream(DF_TERM_PTR, DF_TABIND);

//empty list
void DF_MkNil_(DF_TERM_PTR);
DF_TERM_PTR DF_MkNil(DF_TERM_PTR);

//reference
void DF_MkRef_(DF_TERM_PTR, DF_TERM_PTR);
DF_TERM_PTR DF_MkRef(DF_TERM_PTR, DF_TERM_PTR);

//list cons
void DF_MkCons_(DF_TERM_PTR, DF_TERM_PTR);
DF_TERM_PTR DF_MkCons(DF_TERM_PTR, DF_TERM_PTR);

//abstraction
void DF_MkLam_(DF_TERM_PTR, DF_EMBEDLEV, DF_TERM_PTR);
DF_TERM_PTR DF_MkLam(DF_TERM_PTR, DF_EMBEDLEV, DF_TERM_PTR);

//application
void DF_MkApp_(DF_TERM_PTR, DF_ARITY, DF_TERM_PTR, DF_TERM_PTR); 
DF_TERM_PTR DF_MkApp(DF_TERM_PTR, DF_ARITY, DF_TERM_PTR, DF_TERM_PTR);

//suspension
void DF_MkSusp_(DF_TERM_PTR, DF_EMBEDLEV, DF_EMBEDLEV, DF_TERM_PTR, DF_ENV_PTR);
DF_TERM_PTR DF_MkSusp(DF_TERM_PTR, DF_EMBEDLEV, DF_EMBEDLEV, DF_TERM_PTR, 
                      DF_ENV_PTR);

//pair environment item
void DF_MkEnv_(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV, DF_TERM_PTR); 
DF_ENV_PTR DF_MkEnv(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV, DF_TERM_PTR);

//dummy environment item
void DF_MkDummyEnv_(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV); 
DF_ENV_PTR DF_MkDummyEnv(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV);


/***********************************************************/
/*interface functions for in-place updating certain fields */
/***********************************************************/

//updating universe index
void DF_ModUnivIndex(DF_TERM_PTR, DF_UNIVIND);


/***************************************************************************/
/*interface functions for addr calculation, may be needed for detecting    */
/*memory error                                                             */
/* (some are not implemented yet)                                          */
/***************************************************************************/

//return an address increased by the size of an atomic term from the given one
DF_TERM_PTR DF_IncAtomic(DF_TERM_PTR);

//return an address increased by the size of an app head from the given one
DF_TERM_PTR DF_IncAppHead(DF_TERM_PTR);


//return an address increased by the size of a suspension from the given one
DF_TERM_PTR DF_IncSusp(DF_TERM_PTR);

//return an address increased by the size of a dummy env item from the given one
DF_ENV_PTR DF_IncEnvDummy(DF_ENV_PTR);

//return an address increased by the size of a pair env item from the given one
DF_ENV_PTR DF_IncEnvPair(DF_ENV_PTR);


//increasing sizeof(APP)+ n*atomic size, where n is arity
DF_TERM_PTR DF_IncAppNArgs(DF_TERM_PTR, DF_ARITY n);


//increasing sizeof(SUSP)*n
DF_TERM_PTR accAddrNSUSP(DF_TERM_PTR, int n);

//increasing sizeof(TCONST)
DF_TERM_PTR accAddrTCONST(DF_TERM_PTR); //may need ifdef if diff treatment 
                                        //will be used for TCONST depending 
                                        //on machine architecture


/*********************************************/
/* term dereference                          */
/*********************************************/
DF_TERM_PTR DF_Deref(DF_TERM_PTR);


/* limits on field values that are changable at runtime : for error checking */
#define  DF_MAXDBIND    USHRT_MAX   //max de Bruijn index (embedding level)
#define  DF_MAXUINVIND  USHRT_MAX   //max universe index
#define  DF_MAXARITY    USHRT_MAX   //max arity 
#define  DF_MAXTABIND   UINT_MAX    //max symbol table index
#define  DF_MAXINT      LONG_MAX    //max integer
#define  DF_MININT      LONG_MIN    //min integer

    
/* Attributes of some special constants */
#define  DF_CONSARITY  2         //arity of cons

 
#endif // DATAFORMAT_H
    





