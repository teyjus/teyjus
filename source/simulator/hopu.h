/****************************************************************************/
/*  File hopu.h. This header file defines the interface components for the  */
/*  code in hopu.c that implements higher-order pattern unification.        */
/*                                                                          */
/****************************************************************************/
#ifndef HOPU_H
#define HOPU_H

#include "mctypes.h"
#include "dataformats.h"

extern Boolean HOPU_copyFlagGlb;

DF_TermPtr HOPU_lamBody(DF_TermPtr tPtr);

void       HOPU_globalizeCopyRigid(DF_TermPtr rPtr, DF_TermPtr vPtr);
DF_TermPtr HOPU_globalizeFlex(DF_TermPtr fPtr);


DF_TermPtr HOPU_flexNestedSubstC(DF_TermPtr fhPtr, DF_TermPtr args, int nargs,
                                 DF_TermPtr tmPtr, int emblev);
DF_TermPtr HOPU_rigNestedSubstC(DF_TermPtr rhPtr, DF_TermPtr rPtr, 
                                DF_TermPtr args, int rnargs, int emblev);

void HOPU_patternUnify();
void HOPU_patternUnifyPair();





#endif //HOPU_H
