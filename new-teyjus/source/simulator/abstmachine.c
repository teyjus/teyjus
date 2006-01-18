/*temp */

#ifndef ABSTMACHINE_C
#define ASBTMACHINE_C

#include  "mctypes.h"
#include  "dataformat.h"
#include  "abstmachine.h"

//to be removed: temp for exception handling
#include  <stdio.h>
#include  <stdlib.h>

MEM_PTR        AM_hreg;    //heap top


DF_TERM_PTR    AM_vbbreg;        /* variable being bound (for occ chk)     */
  

FLAG           AM_consflag;  // is term a cons? (set in hnorm)
FLAG           AM_rigflag;   // is term rigid? (set in hnorm)


DF_EMBEDLEV    AM_numabs;   //number of abstraction in hnf
DF_ARITY       AM_numargs;  //number of arguments in hnf

DF_TERM_PTR    AM_head;  //pointing to the head of a hnf (set in hnorm)
DF_TERM_PTR    AM_argvec;//pointing to the args of a hnf (set in hnorm)


/****************************************************************************/
/*                                                                          */
/*      Stack, heap, trail and pdl guard                                    */
/*                                                                          */
/****************************************************************************/



MEM_PTR   AM_heapbeg,   //beginning of the heap
          AM_heapend;   //end of the heap


/****************************************************************************/
/*       Memory overflow error detection                                    */
/****************************************************************************/

//heap overflow
void AM_heapError(MEM_PTR p)
{
    if (AM_heapend < p) {
        // to be replaced by real exception handling functions
        printf("heap over flow\n");
        exit(1);
    }
}

/***************************************************************************/
/*       Other errors                                                      */
/***************************************************************************/

//exceed max number of lambda embeddings
void AM_embedError(DF_PREEMBEDLEV i) 
{
    if (i > DF_MAXBVIND) {
        // to be replaced by real exception handling functions
        printf("exceed the max number of lambda embeddings\n");
        exit(0);
    }
}

//exceed max numbder of arity
void AM_arityError(DF_PREARITY i)
{
    if (i > DF_MAXARITY){
         // to be replaced by real exception handling functions
        printf("exceed the max number of term arity\n");
        exit(0);
    }
}


#endif //ABSTMACHINE_C

