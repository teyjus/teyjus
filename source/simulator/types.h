/****************************************************************************/
/*                                                                          */
/*  File types.h. This header file identifies the routines defined in       */
/*  types.c that are exported from there. These routines implement          */
/*  operations on types, in particular the interpretive unification on      */
/*  types. These operations are typically needed in the simulator           */
/*  (simulator.c) and higher-order pattern unification (houp.c).            */
/*                                                                          */
/****************************************************************************/
#ifndef TYPES_H
#define TYPES_H

void TY_typesUnify();                       //interpretive unification on types
void TY_pushPairsToPDL(MemPtr, MemPtr, int);//push n pairs of types to PDL

#endif //TYPES_H
