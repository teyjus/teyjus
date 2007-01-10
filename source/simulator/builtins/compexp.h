/****************************************************************************
 *                                                                          *
 * File compexp.h{c}                                                        * 
 *                                                                          *
 * Evaluating comparison:                                                   *
 * term1 <comp op> term2                                                    *
 *                                                                          *
 * The pointers to term1, term2 are in REG(1) and REG(2).                   *
 *                                                                          *
 * Approach:                                                                *
 * We don't want to repeat work already done in BIEVAL_eval(), and we don't *
 * want to maintain two versions of evaluation.  The implementation of "is" *
 * provides three functions (evalInt, evalFloat, evalString)                *
 * which are used in the current routine.                                   *
 ****************************************************************************/

#ifndef COMPEXP_H
#define COMPEXP_H

void BICOMP_comp();

#endif  //COMPEXP_H
