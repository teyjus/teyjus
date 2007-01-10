/****************************************************************************
 *                                                                          *
 *  File io-datastructures.c.                                               *
 *                                                                          *
 ****************************************************************************/
#include "io-datastructures.h"

/* The io free term variable table */
IO_FreeVarInfo IO_freeVarTab[IO_MAX_FREE_VARS];

/* index for the topmost cell that has been used */
int IO_ftabTop;

/* initialize */
void IO_initIO()
{
    IO_ftabTop = 0;
}
