#ifndef MEMORY_C
#define MEMORY_C

#include "memory.h"
#include "../simulator/mctypes.h"        //to be changed
#include "../simulator/dataformats.h"    //to be changed


/**********************************************************************/
/*      KIND SYMBOL TABLE                                             */
/**********************************************************************/
typedef struct          //TO BE DECIDED
{
    char   *name;
    int    arity;    
} MEM_KstEntry;

/**********************************************************************/
/*      CONSTANT SYMBOL TABLE                                         */
/**********************************************************************/
MemPtr MEM_cstBase;                     //starting addr of const symbol table

// TO BE DECIDED
typedef struct          
{
    char     *name;
    int      typeEnvSize;
    DF_Type  typeSkel;
    // needness vector
    int      univCount;
    int      precedence;
    int      fixity;
} MEM_CstEntry;

#define MEM_CST_ENTRY_SIZE         7    //size (in word) of each entry


MEM_CstEntry *MEM_cstCurrent;           //current cst top

void MEM_cstInit(MemPtr base)
{
    MEM_cstBase    = base;
    MEM_cstCurrent = (MEM_CstEntry*)(base);
}

void MEM_cstSimInit(MemPtr base)
{
    MEM_cstBase = base;
}


void MEM_cstInsert(char* name, int typeEnvSize, DF_Type typeSkel, 
                   int uc, int pred, int fixity)
{
    MEM_cstCurrent -> name = name;
    MEM_cstCurrent -> typeEnvSize = typeEnvSize;
    MEM_cstCurrent -> typeSkel = typeSkel;
    MEM_cstCurrent -> univCount = uc;
    MEM_cstCurrent -> precedence = pred;
    MEM_cstCurrent -> fixity = fixity;

    MEM_cstCurrent=(MEM_CstEntry*)(((MemPtr)MEM_cstCurrent)+MEM_CST_ENTRY_SIZE);
}

char*   MEM_cstName(int n){             //name of the const in the nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> name;
}

int     MEM_cstTyEnvSize(int n){        //type env size in the nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> typeEnvSize;
}

int     MEM_cstUnivCount(int n){        //universe count in the nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> univCount;
}
int     MEM_cstPred(int n){             //precedence of the const in nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> precedence;
}
int     MEM_cstFixity(int n){           //fixity of the const in nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> fixity;
}
DF_Type MEM_cstTySkel(int n){           //type skeleton of const in nth entry
    return ((MEM_CstEntry*)(MEM_cstBase+(n*MEM_CST_ENTRY_SIZE))) -> typeSkel;
}


#endif  //MEMORY_C
