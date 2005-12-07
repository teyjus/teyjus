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
    TYPE_TAG_SORT,  //sort
    TYPE_TAG_REF,   //reference
    TYPE_TAG_ARROW, //type arrow
    TYPE_TAG_STR   //type structure
};


//generic type (head) for every category
typedef struct               
{
    DF_TY_TAG        tag;    /* the common field for every type (head); can 
                                be any one of enum TypeCategory.
                                rely on struct alignment */ 
    void*            dummy;  /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TYPE;

typedef DF_TYPE* DF_TYPE_PTR; //type pointer

//interface functions for type recognization

BOOLEAN DF_isTYSORT(DF_TYPE_PTR);  // is sort?

BOOLEAN DF_isTYREF(DF_TYPE_PTR);   // is reference? (including free var)

BOOLEAN DF_isTYFREEVAR(DF_TYPE_PTR);  // is free var?

BOOLEAN DF_isTYARROW(DF_TYPE_PTR);   // is type arrow?

BOOLEAN DF_isTYSTR(DF_TYPE_PTR);     // is type structure?


/*interface functions for type decomposition */
//generic type
DF_TY_TAG DF_tyTag(DF_TYPE_PTR);   // extracting tag

//sorts
DF_TY_TABIND DF_tyKindTableIndex(DF_TYPE_PTR); // extracting kind table index

//arrows
DF_TYPE_PTR DF_tyArrArgs(DF_TYPE_PTR); //extracting addr of args

//structures
DF_TY_TABIND DF_tyStrFunc(DF_TYPE_PTR); //extracting kind table index of functor
DF_TY_ARITY DF_tyStrArity(DF_TYPE_PTR); //extracting arity of functor
DF_TYPE_PTR DF_tyStrArgs(DF_TYPE_PTR); //extracting address of args
DF_TYPE_PTR DF_tyStrFuncArgs(DF_TYPE_PTR);//extracting address of func and args 



/* interface functions for type composition */
//sort
void DF_tyMkSort(DF_TYPE_PTR, DF_TY_TABIND);
DF_TYPE_PTR DF_tyMkSort_p(DF_TYPE_PTR, DF_TY_TABIND);

//reference
void DF_tyMkRef(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_tyMkRef_p(DF_TYPE_PTR, DF_TYPE_PTR);

//free variable
void DF_tyMkFreeVar(DF_TYPE_PTR);
DF_TYPE_PTR DF_tyMkFreeVar_p(DF_TYPE_PTR);

//arrows
void DF_tyMkArr(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_tyMkArr_p(DF_TYPE_PTR, DF_TYPE_PTR);

//structures
void DF_tyMkStr(DF_TYPE_PTR, DF_TYPE_PTR);
DF_TYPE_PTR DF_tyMkStr_p(DF_TYPE_PTR, DF_TYPE_PTR);

//functor can only be created on the heap
DF_TYPE_PTR DF_tyMkFunc(DF_TYPE_PTR, DF_TY_TABIND, DF_TY_ARITY);


/* type dereference */
DF_TYPE_PTR DF_tyDeref(DF_TYPE_PTR);


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
    TERM_TAG_VAR,            // existential variables
    TERM_TAG_CONST,          // constants 
    TERM_TAG_INT,            // integers
    TERM_TAG_FLOAT,          // floats
    TERM_TAG_NIL,            // empty lists
    TERM_TAG_STR,            // strings
    TERM_TAG_BVAR,           // lambda bound variables (de Bruijn index)
    TERM_TAG_STREAM,         // streams
    TERM_TAG_REF,            // references 
                             // all categories above are atomic terms 
    TERM_TAG_CONS,           // list constructors
    TERM_TAG_LAM,            // abstractions
    TERM_TAG_APP,            // applications
    TERM_TAG_SUSP            // suspensions
};

// a generic term (head) for every category
typedef struct               
{
    DF_TAG        tag;       /* the common field for every term (head); can 
                                be any one of enum TermCategory.
                                rely on struct alignment */ 
    void*         dummy;     /* a place holder which enforces the size of the 
                                generic term to be 2 words. */
} DF_TERM;

typedef DF_TERM* DF_TERM_PTR; //term pointer



// environment items (list) in suspension

/* The common fields of ENV and DUMMYENV have to be put in same places with 
   respect to the structures: rely on struct alignment. */
typedef struct DF_env               // pair environment item
{
    struct DF_env*      rest;
    BOOLEAN      isDummy;   // isDummy = 0
    DF_EMBEDLEV  embedLevel;
    DF_TERM_PTR  term;
} DF_ENV;


typedef struct           // dummy environment item
{
    DF_ENV*      rest;
    BOOLEAN      isDummy;   // isDummy = 1
    DF_EMBEDLEV  embedLevel;
    
} DF_DUMMYENV;

typedef DF_ENV* DF_ENV_PTR;

/*********************************************/
/*interface functions for term recognization */
/*********************************************/

BOOLEAN DF_isATOMIC(DF_TERM_PTR);      

BOOLEAN DF_isnATOMIC(DF_TERM_PTR);  /* whether a term is atomic: ref is not
                                    included in atomic terms nor non-atomic
                                    ones */
BOOLEAN DF_isVAR(DF_TERM_PTR);     // is variable?

BOOLEAN DF_isCONST(DF_TERM_PTR);   // is constant (typed and untyped)?

BOOLEAN DF_isINT(DF_TERM_PTR);     // is integer?

BOOLEAN DF_isFLOAT(DF_TERM_PTR);   // is float?

BOOLEAN DF_isNIL(DF_TERM_PTR);     // is empty list?

BOOLEAN DF_isSTR(DF_TERM_PTR);     // is string?

BOOLEAN DF_isBVAR(DF_TERM_PTR);    // is lambda bound variable?

BOOLEAN DF_isSTREAM(DF_TERM_PTR);  // is stream?

BOOLEAN DF_isREF(DF_TERM_PTR);     // is reference?

BOOLEAN DF_isCONS(DF_TERM_PTR);    // is list cons?

BOOLEAN DF_isLAM(DF_TERM_PTR);     // is abstraction?

BOOLEAN DF_isAPP(DF_TERM_PTR);     // is application?

BOOLEAN DF_isSUSP(DF_TERM_PTR);    // is suspension?

//environment item (list)
BOOLEAN DF_empEnv(DF_ENV_PTR);     // is empty environment?

BOOLEAN DF_isDummy(DF_ENV_PTR);    // is dummy environment item?




/*********************************************/
/*interface functions for term decomposition */
/*********************************************/

//generic term
DF_TAG DF_tag(DF_TERM_PTR);        // extracting tag from a generic term

//constants (typed/untyped) and variable
DF_UNIVIND DF_univIndex(DF_TERM_PTR);  // exacting universe index

//constants (typed/untyped)
DF_UNIVIND DF_constUnivIndex(DF_TERM_PTR); //extracting universe index
DF_TABIND DF_constTabIndex(DF_TERM_PTR);   //extracting symbol table index

//typed constants
DF_TYPE_PTR DF_tconstType(DF_TERM_PTR);    //extracting the address of type env

//integer
long DF_intValue(DF_TERM_PTR);             //extracting integer value (long)
long DF_intABS(DF_TERM_PTR);               //absolute value
long DF_intNEG(DF_TERM_PTR);               //negation

//float
float DF_floatValue(DF_TERM_PTR);          //extracting float value
float DF_floatABS(DF_TERM_PTR);            //absolute value
float DF_floatNEG(DF_TERM_PTR);            //negation

//string
char* DF_strValue(DF_TERM_PTR);            //extracting string value
int   DF_strLength(DF_TERM_PTR);           //extracting string length

//stream
DF_TABIND DF_streamTabIndex(DF_TERM_PTR);  //extracting stream index

//lambda bound variable
DF_EMBEDLEV DF_bvIndex(DF_TERM_PTR);       //extracting de Bruijn index

//reference
DF_TERM_PTR DF_refTarget(DF_TERM_PTR);     //extracting the address of target

//list cons
DF_TERM_PTR DF_consArgs(DF_TERM_PTR);   //extracting the address of args of cons

//abstractions
DF_EMBEDLEV DF_lamEmbedLev(DF_TERM_PTR);   //extracting abstraction level
DF_TERM_PTR DF_lamBody(DF_TERM_PTR);    //extracting the address of lambda body

//application
DF_ARITY DF_appArity(DF_TERM_PTR);         //extracting arity
DF_TERM_PTR DF_appFunc_p(DF_TERM_PTR);  //extracting the address of functor
DF_TERM  DF_appFunc(DF_TERM_PTR);          //extracting the functor
DF_TERM_PTR DF_appArgs(DF_TERM_PTR);    //extracting the address of arg vector

//suspension
DF_EMBEDLEV DF_suspOL(DF_TERM_PTR);        //extracting ol
DF_EMBEDLEV DF_suspNL(DF_TERM_PTR);        //extracting nl
DF_TERM_PTR DF_suspTermSkel(DF_TERM_PTR);  //extracting the address of term skel
DF_ENV_PTR  DF_suspEnv(DF_TERM_PTR);       //extracting the environment list


//environment item (dummy/pair)
DF_EMBEDLEV DF_envL(DF_ENV_PTR);           //extracting l in @l or (t,l)

//pair environment item 
DF_TERM_PTR DF_envTerm(DF_ENV_PTR);    //extracting the address of t in (t,l)

//environment item (list) (dummy/pair)
DF_ENV_PTR DF_envListRest(DF_ENV_PTR); //extracting the tail of env list

//environment list 
DF_ENV_PTR DF_envNth(DF_ENV_PTR, int); //extracting the nth item in the env list


/*********************************************/
/*interface functions for term construction  */
/*********************************************/
/*1. void DF_mk<term category> (DF_TERM_PTR tp, info)
     Create a term of corresponding category into the address in tp from
     information given by info;
  2. DF_TERM_PTR DF_mk<term category>_p(DF_TERM_PTR tp, info)
     Create a term of corresponing category into the address in tp from
     information given by info;
     Return a pointer increased by corresponding term size from tp.
     
  *The void functions for terms with sizes larger than atoms may be redundant.
*/ 

//variable
void DF_mkVAR(DF_TERM_PTR, DF_UNIVIND);
DF_TERM_PTR DF_mkVAR_p(DF_TERM_PTR, DF_UNIVIND);

//lambda bound variable
void DF_mkBV(DF_TERM_PTR, DF_EMBEDLEV);
DF_TERM_PTR DF_mkBV_p(DF_TERM_PTR, DF_EMBEDLEV);

//untyped constant
void DF_mkCONST(DF_TERM_PTR, DF_UNIVIND, DF_TABIND);
DF_TERM_PTR DF_mkCONST_p(DF_TERM_PTR, DF_UNIVIND, DF_TABIND);

//typed constant
void DF_mkTCONST(DF_TERM_PTR, DF_UNIVIND, DF_TABIND, DF_TYPE_PTR); 
DF_TERM_PTR DF_mkTCONST_p(DF_TERM_PTR, DF_UNIVIND, DF_TABIND, DF_TYPE_PTR);

//integer
void DF_mkINT(DF_TERM_PTR, long);
DF_TERM_PTR DF_mkINT_p(DF_TERM_PTR, long);

//float
void DF_mkFLOAT(DF_TERM_PTR, float);
DF_TERM_PTR DF_mkFLOAT_p(DF_TERM_PTR, float);

//string
void DF_mkSTR(DF_TERM_PTR, char*);
DF_TERM_PTR DF_mkSTR_p(DF_TERM_PTR, char*);

//stream
void DF_mkSTREAM(DF_TERM_PTR, DF_TABIND);
DF_TERM_PTR DF_mkSTREAM_p(DF_TERM_PTR, DF_TABIND);

//empty list
void DF_mkNIL(DF_TERM_PTR);
DF_TERM_PTR DF_mkNIL_p(DF_TERM_PTR);

//reference
void DF_mkREF(DF_TERM_PTR, DF_TERM_PTR);
DF_TERM_PTR DF_mkREF_p(DF_TERM_PTR, DF_TERM_PTR);

//list cons
void DF_mkCONS(DF_TERM_PTR, DF_TERM_PTR);
DF_TERM_PTR DF_mkCONS_p(DF_TERM_PTR, DF_TERM_PTR);

//abstraction
void DF_mkLAM(DF_TERM_PTR, DF_EMBEDLEV, DF_TERM_PTR);
DF_TERM_PTR DF_mkLAM_p(DF_TERM_PTR, DF_EMBEDLEV, DF_TERM_PTR);

//application
void DF_mkAPP(DF_TERM_PTR, DF_ARITY, DF_TERM_PTR, DF_TERM_PTR); 
DF_TERM_PTR DF_mkAPP_p(DF_TERM_PTR, DF_ARITY, DF_TERM_PTR, DF_TERM_PTR);

//suspension
void DF_mkSUSP(DF_TERM_PTR, DF_EMBEDLEV, DF_EMBEDLEV, DF_TERM_PTR, DF_ENV_PTR);
DF_TERM_PTR DF_mkSUSP_p(DF_TERM_PTR, DF_EMBEDLEV, DF_EMBEDLEV, DF_TERM_PTR, 
                        DF_ENV_PTR);

//pair environment item
void DF_mkENV(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV, DF_TERM_PTR); 
DF_ENV_PTR DF_mkENV_p(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV, DF_TERM_PTR);

//dummy environment item
void DF_mkDUMMYENV(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV); 
DF_ENV_PTR DF_mkDUMMYENV_p(DF_ENV_PTR, DF_ENV_PTR, DF_EMBEDLEV);


/*********************************************/
/*interface functions for field modification */
/*********************************************/
void DF_modUnivIndex(DF_TERM_PTR, DF_UNIVIND);


/*********************************************/
/*interface functions for addr calculation   */
/* (not implemented yet)                     */
/*********************************************/

//increasing atomic size
DF_TERM_PTR accAddrATOM(DF_TERM_PTR);

//increasing sizeof(APP)
DF_TERM_PTR accAddrAPP(DF_TERM_PTR);

//increasing sizeof(APP)+ n*atomic size, where n is arity
DF_TERM_PTR accAddrAPPARGS(DF_TERM_PTR, DF_ARITY n);

//increasing sizeof(SUSP)
DF_TERM_PTR accAddrSUSP(DF_TERM_PTR);

//increasing sizeof(SUSP)*n
DF_TERM_PTR accAddrNSUSP(DF_TERM_PTR, int n);

//increasing sizeof(TCONST)
DF_TERM_PTR accAddrTCONST(DF_TERM_PTR); //may need ifdef if diff treatment 
                                        //will be used for TCONST depending 
                                        //on machine architecture


/*********************************************/
/* term dereference                          */
/*********************************************/
DF_TERM_PTR DF_deref(DF_TERM_PTR);



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
    





