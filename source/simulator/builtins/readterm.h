//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#ifndef READ_TERM_H
#define READ_TERM_H

#include "../dataformats.h"

void R_ReadTermAndType(DF_TermPtr tmLoc, DF_TypePtr tyLoc);

/**************************************************************************/
/* Term/type creation functions invoked from OCaml Readterm module        */
/**************************************************************************/
/* set the type starting location */
void RT_setTypeStart(MemPtr loc);
/* set the term starting location */
void RT_setTermStart(MemPtr loc);
DF_TermPtr RT_getTermStart();
DF_TypePtr RT_getTypeStart();

// NG: No longer in use, since queries are now compiled
/* initialize local free variable address table, free type variable address
   table, term address table and type address table */
/* int RT_initLocalTabs(int numFvs,int numTyFvs,int numTermArgs,int numTypeArgs); */

/* For compiled queries, the only input are variables, so
   no need to initialize term and type queues */
int RT_initLocalTabsQuery(int numFvs);

/* reclaim local tables */
void RT_cleanLocalTabs();

/* create free variable */
int RT_buildFreeVar(char* name, int index);
/* create free type variable */
int RT_buildFreeTypeVar(int index);

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
int RT_buildIntTerm(int i);
int RT_buildRealTerm(double f);
int RT_buildStringTerm(char* str);
int RT_buildNilTerm();
int RT_buildMConstantTerm(int index);
int RT_buildPConstantTerm(int index, int tyEnvSize);
int RT_buildFreeVarTerm(int index);
int RT_buildDBTerm(int index);
int RT_buildAbstractionTerm(int numAbs);
int RT_buildConsTerm();
int RT_buildApplicationTerm(int arity);


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
int RT_buildArrowType();
int RT_buildSortType(int index);
int RT_buildStrType(int index, int arity);
int RT_buildFreeVarType(int index);

#endif /* READ_TERM_H */
