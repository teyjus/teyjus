/****************************************************************************/
/*                                                                          */
/*   File  abstmachine.h. This header file defines the various registers,   */
/*   data areas and record types relevant to the abstract machine.          */
/*                                                                          */
/****************************************************************************/
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
extern MemPtr       AM_pdlTop;              //top of pdl
extern MemPtr       AM_typespdlBot;         //(moving) bottom of types pdl

extern Flag         AM_consFlag;            //cons? 
extern Flag         AM_rigFlag;             //rigid? 


extern TwoBytes     AM_numAbs;              //number of abstractions in hnf
extern TwoBytes     AM_numArgs;             //number of arguments in hnf

extern DF_TermPtr   AM_head;                //head of a hnf 
extern DF_TermPtr   AM_argVec;              //argument vector of a hnf 

extern DF_TermPtr   AM_vbbreg;              //variable being bound for occ
extern DF_TypePtr   AM_tyvbbreg;            //type var being bound for occ

/****************************************************************************/
/*               STACK, HEAP, TRAIL AND PDL RELATED STUFF                   */
/****************************************************************************/
extern MemPtr    AM_heapBeg,                //beginning of the heap
                 AM_heapEnd,                //end of the heap
                 AM_pdlBeg,                 //beginning of pdl
                 AM_pdlEnd;                 //end of pdl


/***************************************************************************/
/*                        PDL OPERATIONS                                   */
/***************************************************************************/ 
MemPtr   AM_popPDL();                       //pop (term/type) PDL
void     AM_pushPDL(MemPtr);                //push (term/type) PDL

Boolean  AM_emptyPDL();                     //is empty term PDL?
Boolean  AM_nemptyPDL();                    //is not empty term PDL?

Boolean  AM_emptyTypesPDL();                //is empty type PDL?
Boolean  AM_nemptyTypesPDL();               //is not empty type PDL?
void     AM_initTypesPDL();                 //initialize type PDL

/****************************************************************************
 *                         OVERFLOW ERROR FUNCTIONS                         *
 ****************************************************************************/
void AM_heapError(MemPtr);                  //heap overflow
void AM_pdlError(int);                      //pdl stack overflow for n cells

/****************************************************************************
 *                     MISCELLANEOUS OTHER ERRORS                           *
 ****************************************************************************/
void AM_embedError(int);    // violation of max number of lambda embeddings
void AM_arityError(int);    // violation of max number of arity in applications

#endif //ABSTMACHINE_H
