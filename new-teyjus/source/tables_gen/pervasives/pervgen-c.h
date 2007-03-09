/*****************************************************************************/
/* File pervgen-c.h. This files contains function definitions for generating */
/* files pervasives.h and pervasives.c.                                      */
/*****************************************************************************/
#include "types.h"
#include "op.h"

/****************************************************************************/
/* kind relevant components                                                 */
/****************************************************************************/
//kind indices info and kind table info initiation
void cgenKindInit(int size);

//number of pervasive kinds
void cgenNumKinds(char* num);
//pervasive kind indices declaration
void cgenKindIndex(int index, char* name, char* indexT, char* comments);
//pervasive kind relevant information in pervasives.h
void cgenKindH();

//pervasive kind table entries
void cgenKindData(int index, char* name, char* arity, char* comments);
//pervasive kind relevant information in pervasives.c
void cgenKindC();

/****************************************************************************/
/* type skeleton relevant components                                        */
/****************************************************************************/
//number of type skeletons for pervasive constants
void cgenNumTySkels(char* num);
//type skeleton relevant information in pervasives.h
void cgenTySkelsH();

//type skeleton creation code
void cgenTySkelTab(int index, Type tyskel, char* comments);
//type skeleton info initiation
void cgenTySkelInit(int length);
//type skeleton relevant information in pervasives.c
void cgenTySkelsC();


/****************************************************************************/
/* constant relevant components                                             */
/****************************************************************************/
//const indices info and const table info initiation 
void cgenConstInit(int length);

//number of pervasive constants
void cgenNumConsts(char* num);
//pervasive constant indices declaration
void cgenConstIndex(int index, char* name, char* indexT, char* comments);
//pervasive constant relevant information in pervasives.h
void cgenConstH();


//pervasive constant table entries
void cgenConstData(int index, char* name, char* tesize, OP_Prec prec,
                   OP_Fixity fixity, int tySkelInd, char* neededness, 
                   char* comments);
//pervasive const relevant information in pervasives.c
void cgenConstC();

//initiale logic symb types
void cgenLogicSymbolInit(int length);
//generate logic symbol types
void cgenLogicSymbType(int index, char* name, char* indexText);
//generate logic symbol start/end position
void cgenLSRange(char* start, char* end);
//generate predicate symbol start/end position
void cgenPREDRange(char* start, char* end);
void cgenConstProperty();

/****************************************************************************/
/* Writing files                                                            */
/****************************************************************************/
/* dump files pervasives.h   */
void spitCPervasivesH();
/* dump files pervasives.c   */
void spitCPervasivesC();
