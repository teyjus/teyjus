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
#include   "../system/memory.h"  //to be changed

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

//The size of AM_adjreg is decided by that of relevant fields in term
//representations which can be found in dataformats.c
TwoBytes     AM_adjreg;              //univ count of variable being bound

DF_DisPairPtr AM_llreg;              //ptr to the live list

/****************************************************************************/
/*               STACK, HEAP, TRAIL AND PDL RELATED STUFF                  */
/****************************************************************************/

MemPtr     AM_heapBeg,                //beginning of the heap
           AM_heapEnd,                //end of the heap
           AM_pdlBeg,                 //beginning of pdl
           AM_pdlEnd;                 //end of pdl


/***************************************************************************/
/*                     LIVE LIST OPERATIONS                                */
/***************************************************************************/
//live list is empty?
Boolean AM_empLiveList() { return (AM_llreg == DF_EMPTY_DIS_SET);}

//live list not empty?
Boolean AM_nempLiveList(){ return (AM_llreg != DF_EMPTY_DIS_SET);}

//add a dis pair to the live list when not knowning it is empty or not
void    AM_addDisPair(DF_TermPtr tPtr1, DF_TermPtr tPtr2)
{
    MemPtr nhtop = AM_hreg + DF_DISPAIR_SIZE;
    AM_heapError(nhtop);
    if (AM_nempLiveList()) DF_modDisPairPrev(AM_llreg, (DF_DisPairPtr)AM_hreg);
    DF_mkDisPair(AM_hreg, DF_EMPTY_DIS_SET, AM_llreg, tPtr1, tPtr2);
    AM_llreg = (DF_DisPairPtr)AM_hreg;
    AM_hreg = nhtop;
}

//add a dis pair to the live list when knowning it is noempty 
void    AM_addDisPairToNEmp(DF_TermPtr tPtr1, DF_TermPtr tPtr2)
{
    MemPtr nhtop = AM_hreg + DF_DISPAIR_SIZE;
    AM_heapError(nhtop);
    DF_modDisPairPrev(AM_llreg, (DF_DisPairPtr)AM_hreg);
    DF_mkDisPair(AM_hreg, DF_EMPTY_DIS_SET, AM_llreg, tPtr1, tPtr2);
    AM_llreg = (DF_DisPairPtr)AM_hreg;
    AM_hreg = nhtop;
}

//delete a given dis pair from the live list
void    AM_deleteDisPair(DF_DisPairPtr disPtr)
{
    DF_DisPairPtr prevPtr = DF_disPairPrev(disPtr);
    DF_DisPairPtr nextPtr = DF_disPairNext(disPtr);
    //trail (disPtr)
    if (DF_isNEmpDisSet(nextPtr)) DF_modDisPairPrev(nextPtr, prevPtr);
    if (DF_isNEmpDisSet(prevPtr)) DF_modDisPairNext(prevPtr, nextPtr);
    else AM_llreg = nextPtr;
}


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
//recover type PDL to the status before type unification 
void    AM_resetTypesPDL()       { AM_pdlTop = AM_typespdlBot;           }


/****************************************************************************
 *                         OVERFLOW ERROR FUNCTIONS                         *
 ****************************************************************************/
void AM_heapError(MemPtr p)                 //heap overflow
{
    if (AM_heapEnd < p) {
         // to be replaced by real exception handling functions
        printf("heap over flow\n");
        EM_error();
    }
}

void AM_pdlError(int n)                    //pdl overflow for n pairs
{
    if (AM_pdlEnd < (AM_pdlTop + n)){
        // to be replaced by real exception handling functions
        printf("pdl over flow\n");
        EM_error();
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
        EM_error();
    }
}

void AM_arityError(int n)    // violation of max number of arity in applications
{
    if (n > DF_TM_MAX_ARITY){
         // to be replaced by real exception handling functions
        printf("exceed the max number of term arity\n");
        EM_error();
    }
}

/****************************************************************************/
/*                   RUN-TIME SYMBOL TABLES                                 */
/****************************************************************************/
MEM_KstPtr   AM_kstBase;     //starting addr of the kind symbol table
MEM_TstPtr   AM_tstBase;     //starting addr of the type skel table
MEM_CstPtr   AM_cstBase;     //starting addr of the const symbol table

/* Kind symbol table                                                        */
char* AM_kstName(int n)        //name of a type constructor in a given entry
{
    return ((MEM_KstPtr)(((MemPtr)AM_kstBase) + n*MEM_KST_ENTRY_SIZE)) -> name;
}

int   AM_kstArity(int n)       //arity of a type constructor in a given entry
{
    return ((MEM_KstPtr)(((MemPtr)AM_kstBase) + n*MEM_KST_ENTRY_SIZE)) -> arity;
}

/* Type skeleton table                                                      */
DF_TypePtr AM_tstSkel(int n)   //type skeleton in a given entry
{
    return (DF_TypePtr)(((MemPtr)AM_tstBase) + n*MEM_TST_ENTRY_SIZE);
}

/* Constant symbol table                                                    */
char* AM_cstName(int n)        //name of a constant in a given entry
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase) + n*MEM_CST_ENTRY_SIZE)) -> name;
}
int   AM_cstTyEnvSize(int n)   //type environment size 
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase)+n*MEM_CST_ENTRY_SIZE))->
        typeEnvSize;
}    
int   AM_cstUnivCount(int n)   //universe count 
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase)+n*MEM_CST_ENTRY_SIZE))->univCount;
}
int   AM_cstPrecedence(int n)  //precedence
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase)+n*MEM_CST_ENTRY_SIZE))->
        precedence;
}
int   AM_cstFixity(int n)      //fixity
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase)+n*MEM_CST_ENTRY_SIZE))->fixity;
}
int   AM_cstTySkelInd(int n)   //type skeleton index
{
    return ((MEM_CstPtr)(((MemPtr)AM_cstBase)+n*MEM_CST_ENTRY_SIZE))->
        tskTabIndex;
}

#endif //ABSTMACHINE_C
