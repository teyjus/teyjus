/*temp */

#ifndef ABSTMACHINE_H
#define ABSTMACHINE_H

#include   "mctypes.h"
#include   "dataformat.h"


typedef  enum {OFF,ON}    AM_FLAG_TYPES; //type for flags


extern MEM_PTR     AM_hreg;     //heap top

extern DF_TERM_PTR AM_vbbreg;   // variable being bound (for occ chk)
  


extern FLAG        AM_consflag; //is term a cons? (set in head normalization)
extern FLAG        AM_rigflag;  //is term rigid? (set in head normalization)

extern DF_EMBEDLEV AM_numabs;  //number of abstractions in hnf
extern DF_ARITY    AM_numargs; //number of arguments in hnf



extern DF_TERM_PTR AM_head;  //pointing to the head of a hnf (set in hnorm)
extern DF_TERM_PTR AM_argvec;//pointing to the args of a hnf (set in hnorm)



/****************************************************************************/
/*                                                                          */
/*      Stack, heap, trail and pdl guard                                    */
/*                                                                          */
/****************************************************************************/



extern MEM_PTR   AM_heapbeg,   //beginning of the heap
                 AM_heapend;   //end of the heap



/****************************************************************************/
/*       Memory overflow error detection                                    */
/****************************************************************************/
void AM_heapError(MEM_PTR);

void AM_embedError(DF_PREEMBEDLEV);

void AM_arityError(DF_PREARITY);


#endif //ABSTMACHINE_H


