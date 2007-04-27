/****************************************************************************
 *                                                                          *
 *  File io-datastructures.c.                                               *
 *                                                                          *
 ****************************************************************************/
#include "io-datastructures.h"

/* The io free term variable table */
IO_FreeVarInfo IO_freeVarTab[IO_MAX_FREE_VARS];

/* index for the topmost cell that has been used */
int IO_freeVarTabTop;

/* initialize */
void IO_initIO()
{
    IO_freeVarTabTop = 0;
}

/* check if the free term variable table is full */
Boolean IO_freeVarTabFull(int incSize)  
{
    return (IO_freeVarTabTop+incSize >= IO_MAX_FREE_VARS);
}

/* make an entry in the free term variable table */
void IO_enterFreeVarTab(DF_StrDataPtr name, DF_TermPtr varLoc)
{
    int i = IO_freeVarTabTop++;
    
    IO_freeVarTab[i].varName  = name;
    IO_freeVarTab[i].rigdes = varLoc;
}

