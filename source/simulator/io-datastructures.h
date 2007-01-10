/****************************************************************************
 *                                                                          *
 *  File io-datastructures.h.                                               *
 *                                                                          *
 ****************************************************************************/

#ifndef IODATASTRUCTURES_H
#define IODATASTRUCTURES_H

#include "mcstring.h"
#include "dataformats.h"

/*****************************************************************************
 * A data structure for maintaining information about query term variables   *
 * and other free variables encountered in the course of displaying answers. *
 *****************************************************************************/
/* number of entries in the table for such variables. */
#define IO_MAX_FREE_VARS   500

/* Structure of each entry in the table; display name, and the rigid
   designator in the form of the memory cell corresponding to the variable are
   maintained. */
typedef struct 
{
    MCSTR_Str   varName;
    DF_TermPtr  rigdes;
} IO_FreeVarInfo;

/* The table itself */
extern IO_FreeVarInfo IO_freeVarTab[IO_MAX_FREE_VARS];

/* index for the topmost cell that has been used */
extern int IO_ftabTop;

/* initialize */
void IO_initIO();


#endif  //IODATASTRUCTURES_H
