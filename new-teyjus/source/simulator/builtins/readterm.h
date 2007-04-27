#ifndef READ_TERM_H
#define READ_TERM_H

#include "../dataformats.h"

void R_ReadTermAndType(DF_TermPtr tmLoc, DF_TypePtr tyLoc);

/**************************************************************************/
/* Term/type creation functions invoked from OCaml Readterm module        */
/**************************************************************************/
/* initialize local free variable address table, free type variable address
   table, term address table and type address table */
void RT_initLocalTabs(int numFvs,int numTyFvs,int numTermArgs,int numTypeArgs);
/* reclaim local tables */
void RT_cleanLocalTabs();

/* create free variable */
void RT_buildFreeVar(char* name, int index);

/* create free type variable */
void RT_buildFreeTypeVar(int index);

/***************************************************************************/
/* Creating term on the heap:                                              */
/* If the term is:                                                         */
/* a) free variable:                                                       */
/*    a reference to the address in the ith entry (where i is the given    */
/*    index) of the local free variable table is created at the location   */
/*    dequeued from the term queue;                                        */
/* b) one with atomic size:                                                */
/*    the term is created at the location dequeued from the term queue; if */
/*    the term is an abstraction, then the address of its body is enqueued */
/* c) application:                                                         */
/*    the term is created at the current heap top; a reference to it is    */
/*    made at the location dequeued from the term queue; the addresses of  */
/*    the function and arguments are enqueued;                             */
/* b) constant with type association:                                      */
/*    the term is created at the current heap top; a reference to it is    */
/*    made at the location dequeued from the term queue; the addresses of  */
/*    its type environments are enqueued into the type queue.              */
/***************************************************************************/
void RT_buildIntTerm(int i);
void RT_buildRealTerm(double f);
void RT_buildStringTerm(char* str);
void RT_buildNilTerm();
void RT_buildMConstantTerm(int index);
void RT_buildPConstantTerm(int index, int tyEnvSize);
void RT_buildFreeVarTerm(int index);
void RT_buildDBTerm(int index);
void RT_buildAbstractionTerm(int numAbs);
void RT_buildConsTerm();
void RT_buildApplicationTerm(int arity);


/***************************************************************************/
/* Creating type on the heap:                                              */
/* If the type is:                                                         */
/* a) free variable:                                                       */
/*    a reference to the address in the ith entry (where i is the given    */
/*    index) of the local free ty variable table is created at the location*/
/*    dequeued from the type queue;                                        */
/* b) others:                                                              */
/*    a type is created at the location dequeued from the type queue with  */
/*    the argument vector on the current heap top; the addresses of        */
/*    arguments are enqueued.                                              */
/***************************************************************************/
void RT_buildArrowType();
void RT_buildSortType(int index);
void RT_buildStrType(int index, int arity);
void RT_buildFreeVarType(int index);

#endif /* READ_TERM_H */
