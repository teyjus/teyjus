/*temp */

#ifndef ABSTMACHINE_H
#define ABSTMACHINE_H

#include   "mctypes.h"
#include   "dataformats.h"

/****************************************************************************/
/*                ABSTRACT MACHINE REGISTERS (AND FLAGS)                    */
/****************************************************************************/

typedef enum {OFF,ON}     AM_FlagTypes;     //FLAG type
typedef Byte              Flag;                       


extern MemPtr       AM_hreg;                //heap top

extern Flag         AM_consFlag;            //cons? 
extern Flag         AM_rigFlag;             //rigid? 


extern TwoBytes     AM_numAbs;              //number of abstractions in hnf
extern TwoBytes     AM_numArgs;             //number of arguments in hnf

extern DF_TermPtr   AM_head;                //head of a hnf 
extern DF_TermPtr   AM_argVec;              //argument vector of a hnf 

extern DF_TermPtr   AM_vbbreg;              //variable being bound for occ
/****************************************************************************/
/*               STACK, HEAP, TRAIL AND PDL RELATED STUFF                   */
/****************************************************************************/

extern MemPtr    AM_heapBeg,                //beginning of the heap
                 AM_heapEnd;                //end of the heap


/****************************************************************************
 *                         OVERFLOW ERROR FUNCTIONS                         *
 ****************************************************************************/
void AM_heapError(MemPtr);                 //heap overflow


/****************************************************************************
 *                     MISCELLANEOUS OTHER ERRORS                           *
 ****************************************************************************/

/* violation of max number of lambda embeddings */
void AM_embedError(int);

/* violation of max number of arity in applications */
void AM_arityError(int);

#endif //ABSTMACHINE_H
