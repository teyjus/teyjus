//TEMP: should be automatically generated

#ifndef PERVASIVES_H
#define PERVASIVES_H

#include "../simulator/mctypes.h"      //to be changed
#include "../simulator/dataformats.h"  //to be changed

/****************************************************************************/
/*   PERVASIVE KIND                                                         */
/****************************************************************************/

//indices for predefined sorts and type constructors
typedef enum PERV_KindIndexType
{
    PERV_INT_INDEX = 0,                 //int
    PERV_REAL_INDEX,                    //real
    PERV_BOOL_INDEX,                    //bool
    PERV_STRING_INDEX,                  //string
    PERV_LIST_INDEX,                    //list type constructor
    PERV_INSTREAM_INDEX,                //in_stream
    PERV_OUTSTREAM_INDEX                //out_stream
} PERV_KindIndexType;

//total number of pervasive kinds
#define PERV_KIND_NUM                   7

//pervasive kind data type 
typedef struct
{
    char         *name;
    TwoBytes     arity;  
} PERV_KindData;

//pervasive kind data table (array)
extern PERV_KindData    PERV_kindDataTab[PERV_KIND_NUM];

//pervasive kind data access function 
PERV_KindData PERV_getKindData(int index);

//pervasive kind table copy function (used in module space initialization)
//this functiion relies on the assumption that the pervasive kind data
//has the same structure as that of the run-time kind symbol table entries.
void PERV_copyKindDataTab(PERV_KindData* dst);

/****************************************************************************/
/*   TYPE SKELETIONS FOR PERVASIVE CONSTANTS                                */
/****************************************************************************/

//total number of type skeletons needed for pervasive constants
//#define PERV_TY_SKEL_NUM                 41
#define PERV_TY_SKEL_NUM                   10

//pervasive type skel data type
typedef DF_TypePtr  PERV_TySkelData;

//pervasive type skel table (array)
extern  PERV_TySkelData   PERV_tySkelTab[PERV_TY_SKEL_NUM];

//pervasive type skeletons and type skeleton table initialization
//Note that type skeltons have to be dynamically allocated, and so does the
//info recorded in each entry of the pervasive type skeleton table
void PERV_tySkelTabInit();

//pervasive tyskel table copy function
void PERV_copyTySkelTab(PERV_TySkelData* dst);


/***************************************************************************/
/*   PERVASIVE CONSTANTS                                                   */
/***************************************************************************/

//indices for prevasive constants
typedef enum PERV_ConstIndexType
{
    /*logical constants. The first five constants are assumed to appear in
      succession; this is important to ``interpretation'' of complex goals. */
    PERV_AND_INDEX         = 0,            //logical and
    PERV_OR_INDEX          = 1,            //logical or
    PERV_SOME_INDEX        = 2,            //existential quantifier 
    PERV_ALL_INDEX         = 3,            //universal quantifier
    PERV_TRUE_INDEX        = 4,            //true proposition
    PERV_CUT_INDEX         = 5,            //cut predicate
    PERV_FAIL_INDEX        = 6,            //fail predicate
                                           //empty
    PERV_AMPAND_INDEX      = 8,            //another logical and
    PERV_HALT_INDEX        = 9,            //halt the system
    PERV_STOP_INDEX        = 10,           //return to toplevel
    PERV_COLONDASH_INDEX   = 11,           //Prolog if; needed?
    PERV_IMPL_INDEX        = 12,           //implication; needed?

    /* Pervasives with boolean target type; need to be contiguous for 
       convenience in implementing interpreted goals */

    PERV_INTLS_INDEX       = 15,           //less than on integers
    PERV_INTGT_INDEX       = 16            //greater than on integers 

    //... 
} PERV_ConstIndexType;

//total number of pervasive kinds
//#define PERV_CONST_NUM                   92
#define PERV_CONST_NUM                     17

//pervasive const data type 
typedef struct
{
    char      *name;
    TwoBytes  typeEnvSize;
    TwoBytes  tskTabIndex;     //index to the type skeleton table 
    //word      neednessVec;   //needness vector (predicate constant)
    TwoBytes  univCount;
    int       precedence;
    int       fixity;
} PERV_ConstData;

//pervasive const data table (array)
extern PERV_ConstData    PERV_constDataTab[PERV_CONST_NUM];

//pervasive const data access function 
PERV_ConstData PERV_getConstData(int index);

//pervasive const table copy function (used in module space initialization)
//this functiion relies on the assumption that the pervasive kind data
//has the same structure as that of the run-time kind symbol table entries.
void PERV_copyConstDataTab(PERV_ConstData* dst);

//functions used by the simulator for interpreted goals
Boolean PERV_isLogicSymb(int index);
Boolean PERV_isPredSymb(int index);

typedef enum PERV_LogicSymbTypes
{
    PERV_AND     = 0,
    PERV_OR      = 1,
    PERV_SOME    = 2,
    PERV_ALL     = 3,
    PERV_LTRUE   = 4,
    PERV_CUT     = 5,
    PERV_FAIL    = 6,
    PERV_EQ      = 7,
    PERV_AMPAND  = 8,
    PERV_HALT    = 9,
    PERV_STOP    = 10
} PERV_LogicSymbTypes;

PERV_LogicSymbTypes PERV_logicSymb(int index);


#endif  //PERVASIVES_H
