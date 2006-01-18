/****************************************************************************/
/*                                                                          */
/* File hnorm.h.                                                            */
/* This header file identifies routines defined in hnorm.c that are         */
/* exported from there.                                                     */
/****************************************************************************/
#ifndef HNORM_H
#define HNORM_H

#include "dataformat.h"

/* head normalization of the term in the argument */
void HN_hnorm(DF_TERM_PTR); 

/* head normalization of the term in the argument with occurs-check */
void HN_hnormWithOCC(DF_TERM_PTR);

/* full normalization of the term in the argument */
void HN_lnorm(DF_TERM_PTR);


#endif //HNORM_C
