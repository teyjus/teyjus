//TEMP: should be automatically generated

#ifndef PERVASIVES_C
#define PERVASIVES_C

#include <string.h>
#include "pervasives.h"
#include "../system/error.h"     //to be changed
#include "../system/operators.h" //to be changed

#include <stdio.h>
#include "../simulator/print.h"

/****************************************************************************/
/*   PERVASIVE KIND                                                         */
/****************************************************************************/

//pervasive kind data table (array)
PERV_KindData   PERV_kindDataTab[PERV_KIND_NUM] = {
   //name,            arity
    {"int",            0},        //int
    {"real",           0},        //real
    {"o",              0},        //bool
    {"string",         0},        //string
    {"list",           1},        //list type constructor
    {"in_stream",      0},        //in_stream
    {"out_stream",     0}         //out_stream
};


PERV_KindData PERV_getKindData(int index)
{
    return PERV_kindDataTab[index];
}
void PERV_copyKindDataTab(PERV_KindData* dst)
{
    //this way of copy relies on the assumption that the pervasive kind data
    //has the same structure as that of the run-time kind symbol table entries.
    memcpy((void*)dst, (void*)PERV_kindDataTab, 
           sizeof(PERV_KindData) * PERV_KIND_NUM);
}

/****************************************************************************/
/*   TYPE SKELETIONS FOR PERVASIVE CONSTANTS                                */
/****************************************************************************/

//pervasive type skeleton table (array)
PERV_TySkelData   PERV_tySkelTab[PERV_TY_SKEL_NUM];

//pervasive type skeletons and type skeleton table initialization
//The type skeletons are created in the memory of the system through malloc,
//and addresses are entered into the pervasive type skeleton table. 
void PERV_tySkelTabInit()
{
    int tySkelInd = 0; //ts tab index
    MemPtr tySkelBase = (MemPtr)EM_malloc(WORD_SIZE * 64); //ts area
    
    //skeleton var 0
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    
    //(list A)
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkStrType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkStrFuncType(tySkelBase, (int)PERV_LIST_INDEX, 1);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    
    //A -> (list A) -> (list A)
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkStrType(tySkelBase, (DF_TypePtr)(tySkelBase + 2*DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkStrType(tySkelBase, (DF_TypePtr)(tySkelBase + 3*DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkStrFuncType(tySkelBase, (int)PERV_LIST_INDEX, 1);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkStrFuncType(tySkelBase, (int)PERV_LIST_INDEX, 1);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;

    //int
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkSortType(tySkelBase, (int)PERV_INT_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
       
    //real
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkSortType(tySkelBase, (int)PERV_REAL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;

    //string
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkSortType(tySkelBase, (int)PERV_STRING_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    
    //o
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkSortType(tySkelBase, (int)PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;

    //...

    // int -> int -> o
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_INT_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_INT_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;


    // o -> o -> o
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;

    // (A -> o) -> o
    PERV_tySkelTab[tySkelInd] = (PERV_TySkelData)tySkelBase;
    tySkelInd++;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkArrowType(tySkelBase, (DF_TypePtr)(tySkelBase + 2*DF_TY_ATOMIC_SIZE));
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSkelVarType(tySkelBase, 0);
    tySkelBase += DF_TY_ATOMIC_SIZE;
    DF_mkSortType(tySkelBase, PERV_BOOL_INDEX);
    tySkelBase += DF_TY_ATOMIC_SIZE;

    
    //...

}

void PERV_copyTySkelTab(PERV_TySkelData* dst)
{
    memcpy((void*)dst, (void*)PERV_tySkelTab, 
           sizeof(PERV_TySkelData) * PERV_KIND_NUM);
}

/***************************************************************************/
/*   PERVASIVE CONSTANTS                                                   */
/***************************************************************************/

//pervasive constant data table (array)
PERV_ConstData   PERV_constDataTab[PERV_CONST_NUM] = {
    //name,           tesize, tst, (need), UC, prec,          fixity
    //PREV_AND_INDEX    
    {",",             0,       8,          0,  OP_INFIXL,     110}, 
    //PERV_OR_INDEX
    {";",             0,       8,          0,  OP_INFIXL,     100},        
    //PERV_SOME_INDEX
    {"sigma",         1,       9,          0,  OP_NONE,       0  },
    //PERV_ALL_INDEX
    {"pi",            1,       9,          0,  OP_NONE,       0  },
    //PERV_TRUE_INDEX
    {"true",          0,       6,          0,  OP_NONE,       0  },
    //PERV_CUT_INDEX
    {"!",             0,       6,          0,  OP_NONE,       0  },
    //PERV_FAIL_INDEX
    {"fail",          0,       6,          0,  OP_NONE,       0  },
    //<nothing>
    {NULL,            0,       0,          0,  OP_NONE,       0  },
    //PERV_AMPAND_INDEX
    {"&",             0,       8,          0,  OP_INFIXR,     120},
    //PERV_HALT_INDEX
    {"halt",          0,       7,          0,  OP_NONE,       0  },
    //PERV_STOP_INDEX
    {"stop",          0,       6,          0,  OP_NONE,       0  },
    //PERV_COLONDASH_INDEX
    {":-",            0,       8,          0,  OP_INFIXL,     0  },
    //PERV_IMPL_INDEX
    {"=>",            0,       8,          0,  OP_INFIXR,     130},
    //<nothing>
    {NULL,            0,       0,          0,  OP_NONE,       0  },
    //<nothing>
    {NULL,            0,       0,          0,  OP_NONE,       0  },
    //PERV_INTLS_INDEX
    {"<",             0,       7,          0,  OP_INFIX,      130},
    //PERV_INTGT_INDEX
    {">",             0,       7,          0,  OP_INFIX,      130}
    //...
};

PERV_ConstData PERV_getConstData(int index)
{
        return PERV_constDataTab[index];
}

void PERV_copyConstDataTab(PERV_ConstData* dst)
{
    //this way of copy relies on the assumption that the pervasive kind data
    //has the same structure as that of the run-time kind symbol table entries.
    memcpy((void*)dst, (void*)PERV_constDataTab, 
           sizeof(PERV_ConstData) * PERV_CONST_NUM);
}

#endif //PERVASIVES_C
