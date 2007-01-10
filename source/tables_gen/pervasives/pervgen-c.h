/*****************************************************************************/
/* File pervgen-c.h. This files contains function declarations for generating*/
/* files pervasives.h and pervasives.c.                                      */
/*****************************************************************************/
#include "types.h"

/* generating kind relevant parts */

void kindInfoInit(int size);

//in pervasives.h
void genNumKinds(char* num);
void genKindIndices(int index, char* name, char* indexT, char* comments);
void genKindH(int nonempty);


//in pervasives.c
void genKindData(int index, char* name, char* arity, char* comments);
void genKindC();
 
/* generating type skeleton relevant parts */

void tySkelInfoInit(int size);

//in pervasives.h
void genNumTySkels(char* num);
void genTySkelH();

//in pervasives.c
void genTySkels(int index, Type tySkelAbs, char* comments);
void genTySkelC();


/* generating constant relevant parts */

void constInfoInit(int size);
void lsInfoInit(int size);


//in pervasives.h
void genNumConsts(char* num);
void genConstIndices(int index, char* name, char* indexT, char* comments);
void genConstEmptyIndices(int index);
void genConstH();

void genLogicSymbTypes(int index, char* name, char* indexT);
void genLSRange(char* start, char* end);
void genPREDRange(char* start, char* end);


//in pervasives.c
void genConstData(int index, char* name, char* tesize, char* prec, char* fixity,
                  int tySkelInd, char* comments, char* neededness);
void genConstEmptyData(int index);
void genConstC();



/* dump files pervasives.h pervasives.c    */
void spitCPervasivesH();
void spitCPervasivesC();
