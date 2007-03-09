/*****************************************************************************/
/* File pervgen-ocaml.c. This files contains function definitions for        */
/* generating files pervasive.mli and pervasive.ml.                          */
/*****************************************************************************/
#include "op.h"
#include "types.h"
#include "util.h"

/**************************************************************************/
/* generating pervasive kind relevant part                                */
/**************************************************************************/
void ocamlGenKind(char* kindName, char* kVarName, char* arity);
void ocamlGenKinds();

/**************************************************************************/
/* generating pervasive type skeleton relevant part                       */
/**************************************************************************/
void ocamlGenTySkel(char* ind, Type tySkel);

/**************************************************************************/
/* generating pervasive constants relevant part                           */
/**************************************************************************/
void ocamlGenConst(char* ind, char* name, char* cVarName, OP_Fixity fixity, 
                   OP_Prec prec, UTIL_Bool tyPrev, UTIL_Bool redef, int tesize,
                   int tyskelInd, int neededness, OP_Code codeInfo);

void ocamlGenConsts();

void ocamlCollectConsts(char* name, int last);

void ocamlGenRC();
void ocamlGenBC();
/***************************************************************************/
/* Dump code into pervasive.ml and pervasive.mli                           */
/***************************************************************************/
/* dump peravsive.ml   */
void spitOCPervasiveML();
/* dump peravsive.mli   */
void spitOCPervasiveMLI();


