/****************************************************************************/
/*                                                                          */
/*   File  abstmachine.c. This file defines the various registers,          */
/*   data areas and record types and their operations  relevant to the      */
/*   abstract machine.                                                      */
/*                                                                          */
/****************************************************************************/
#ifndef ABSTMACHINE_C
#define ABSTMACHINE_C

#include   "mctypes.h"
#include   "dataformats.h"
#include   "abstmachine.h"
#include   "../system/error.h"   //to be changed

//to be removed:
#include   <stdio.h>
/****************************************************************************/
/*                ABSTRACT MACHINE REGISTERS (AND FLAGS)                    */
/****************************************************************************/
MemPtr       AM_hreg;                //heap top
MemPtr       AM_pdlTop;              //top of pdl
MemPtr       AM_typespdlBot;         //(moving) bottom of types pdl

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
DF_TypePtr   AM_tyvbbreg;            //type var being bound for occ
/****************************************************************************/
/*               STACK, HEAP, TRAIL AND PDL RELATED STUFF                  */
/****************************************************************************/

MemPtr     AM_heapBeg,                //beginning of the heap
           AM_heapEnd,                //end of the heap
           AM_pdlBeg,                 //beginning of pdl
           AM_pdlEnd;                 //end of pdl

/***************************************************************************/
/*                        PDL OPERATIONS                                   */
/***************************************************************************/
//pop (term/type) PDL
MemPtr  AM_popPDL()              { return (MemPtr)(*(--AM_pdlTop));      }
//push (term/type) PDL
void    AM_pushPDL(MemPtr addr)  { (*AM_pdlTop++) = (Mem)addr;           }
//is empty term PDL?
Boolean AM_emptyPDL()            { return (AM_pdlTop == AM_pdlBeg);      }
//is not empty term PDL?
Boolean AM_nemptyPDL()           { return (AM_pdlTop > AM_pdlBeg);       }
//is empty type PDL?
Boolean AM_emptyTypesPDL()       { return (AM_pdlTop == AM_typespdlBot); }
//is not empty type PDL?
Boolean AM_nemptyTypesPDL()      { return (AM_pdlTop > AM_typespdlBot);  }
//initialize type PDL
void    AM_initTypesPDL()        { AM_typespdlBot = AM_pdlTop;           }

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

void AM_pdlError(int n)                    //pdl overflow for n pairs
{
    if (AM_pdlEnd < (AM_pdlTop + n)){
        // to be replaced by real exception handling functions
        printf("pdl over flow\n");
        EM_throw();
    }
}   
  
/****************************************************************************
 *                     MISCELLANEOUS OTHER ERRORS                           *
 ****************************************************************************/
void AM_embedError(int n)     //violation of max number of lambda embeddings 
{
    if (n > DF_MAX_BV_IND){
        // to be replaced by real exception handling functions
        printf("exceed the max number of lambda embeddings\n");
        EM_throw();
    }
}

void AM_arityError(int n)    // violation of max number of arity in applications
{
    if (n > DF_TM_MAX_ARITY){
         // to be replaced by real exception handling functions
        printf("exceed the max number of term arity\n");
        EM_throw();
    }
}

#endif //ABSTMACHINE_C
