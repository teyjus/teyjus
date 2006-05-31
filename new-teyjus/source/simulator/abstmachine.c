/*temp */

#ifndef ABSTMACHINE_C
#define ABSTMACHINE_C

#include   "mctypes.h"
#include   "dataformats.h"
#include   "abstmachine.h"

//for debugging -- to be removed
#include   <stdio.h> 

/****************************************************************************/
/*                ABSTRACT MACHINE REGISTERS (AND FLAGS)                    */
/****************************************************************************/
MemPtr       AM_hreg;                //heap top

Flag         AM_consFlag;            //cons?
Flag         AM_rigFlag;             //rigid?

//The size of AM_numAbs is decided by that of relevant fields in term
//representations which can be found in dataformats.c
TwoBytes     AM_numAbs;              //number of abstractions in hnf
//The size of AM_numArgs is decided by that of relevant fields in term
//representations which can be found in dataformats.c
TwoBytes     AM_numArgs;             //number of arguments in hnf

DF_TermPtr   AM_head;                //head of a hnf
DF_TermPtr   AM_argVec;              //argument vector of a hnf 

DF_TermPtr   AM_vbbreg;              //variable being bound for occ

/****************************************************************************/
/*               STACK, HEAP, TRAIL AND PDL RELATED STUFF                  */
/****************************************************************************/

MemPtr     AM_heapBeg,                //beginning of the heap
           AM_heapEnd;                //end of the heap


/****************************************************************************
 *                         OVERFLOW ERROR FUNCTIONS                         *
 ****************************************************************************/
void AM_heapError(MemPtr p)                 //heap overflow
{
    if (AM_heapEnd < p) {
         // to be replaced by real exception handling functions
        printf("heap over flow\n");
        exit(1);
    }
}

/****************************************************************************
 *                     MISCELLANEOUS OTHER ERRORS                           *
 ****************************************************************************/

/* violation of max number of lambda embeddings */
void AM_embedError(int n)
{
    if (n > DF_MAX_BV_IND){
        // to be replaced by real exception handling functions
        printf("exceed the max number of lambda embeddings\n");
        exit(0);
    }
}

/* violation of max number of arity in applications */
void AM_arityError(int n)
{
    if (n > DF_TM_MAX_ARITY){
         // to be replaced by real exception handling functions
        printf("exceed the max number of term arity\n");
        exit(0);
    }
}

#endif //ABSTMACHINE_C
