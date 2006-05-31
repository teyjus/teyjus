/****************************************************************************/
/*                                                                          */
/* File hnorm.h.                                                            */
/* This header file identifies routines defined in hnorm.c that are         */
/* exported from there.                                                     */
/****************************************************************************/
#ifndef HNORM_H
#define HNORM_H

#include "dataformats.h"

/* head normalization of the term in the argument */
void HN_hnorm(DF_TermPtr); 

/* head normalization of the term in the argument with occurs-check */
void HN_hnormOcc(DF_TermPtr);

/* full normalization of the term in the argument */
void HN_lnorm(DF_TermPtr);


#endif //HNORM_H
