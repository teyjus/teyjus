/****************************************************************************/
/*                                                                          */
/*   File trail.h. This header file includes the interface functions        */
/*   for trail operations.                                                  */
/*                                                                          */
/****************************************************************************/
#ifndef TRAIL_H
#define TRAIL_H

#include <stdlib.h>
#include "mctypes.h"
#include "abstmachine.h"
#include "dataformats.h"

/****************************************************************************/
/*       DATA STRUCTURE OF TRAIL ITEMS                                      */
/****************************************************************************/
/* The tags of trail items */
enum TR_TrailDataCategory
{
    TR_TAG_TERM,
    TR_TAG_MULTERM1,
    TR_TAG_MULTERM2,
    TR_TAG_TYPE,
    TR_TAG_MOD
};

/* The leading slot of trail items */
typedef struct 
{
    Byte           tag;         //trial data category tag
    MemPtr         addr;        //the starting address of the trailed item
} TR_TrailItem;

/* The size of the trail item head */
#define TR_TRAIL_ITEM_HEAD_SIZE (int)ceil((double)sizeof(TR_TrailItem)/WORD_SIZE)
/* The sizes of different trail items */
#define TR_TRAIL_TERM_SIZE        TR_TRAIL_ITEM_HEAD_SIZE + DF_TM_ATOMIC_SIZE
#define TR_TRAIL_MULTERM1_SIZE    TR_TRAIL_ITEM_HEAD_SIZE + DF_TM_APP_SIZE
#define TR_TRAIL_MULTERM2_SIZE    TR_TRAIL_ITEM_HEAD_SIZE + DF_TM_SUSP_SIZE
#define TR_TRAIL_TYPE_SIZE        TR_TRAIL_ITEM_HEAD_SIZE
//temp
#define TR_TRAIL_MOD_SIZE         TR_TRAIL_ITEM_HEAD_SIZE + 2

/***************************************************************************/
/*       TRAILING FUNCTIONS                                                */
/***************************************************************************/
void TR_trailTerm(DF_TermPtr addr);     //trailing a term of atomic size
void TR_trailHTerm(DF_TermPtr addr);    //trailing a heap term of atomic size
void TR_trailETerm(DF_TermPtr addr);    //trailing a stack term 
void TR_trailType(DF_TypePtr addr);     //trailing a type (free type variable)
void TR_trailImport(MemPtr addr);       //trailing a backchained field 


/****************************************************************************/
/*       UNWIND TRAIL FUNCTION                                              */
/****************************************************************************/
void TR_unwindTrail(MemPtr trOld);

#endif  //TRAIL_H

