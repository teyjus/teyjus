/****************************************************************************/
/*                                                                          */
/*                                                                          */
/* builtins/evalexp.{c,h} are responsible for evaluating expressions,       */
/* that is, transforming an expression such as (2 + sin 0) to (2).  There   */
/* is an ad-hoc form of polymorphism, supported by the presence of three    */
/* functions, one each for integer, floating-point, and string terms.       */
/*                                                                          */
/****************************************************************************/
#ifndef EVALEXP_H
#define EVALEXP_H

#include "../dataformats.h" //to be modified

void   BIEVAL_eval();

int           BIEVAL_evalInt(DF_TermPtr tmPtr);
float         BIEVAL_evalFloat(DF_TermPtr tmPtr);
DF_StrDataPtr BIEVAL_evalStr(DF_TermPtr tmPtr);


#endif //EVALEXP_H 
