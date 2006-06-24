#ifndef MEMORY_H
#define MEMORY_H

#include <limits.h>
#include "../simulator/mctypes.h"        //to be changed
#include "../simulator/dataformats.h"    //to be changed

/**********************************************************************/
/*      KIND SYMBOL TABLE                                             */
/**********************************************************************/
#define MEM_KST_MAX_IND        USHRT_MAX //TwoBytes DF_KstTabInd (dataformats.c)






/**********************************************************************/
/*      CONSTANT SYMBOL TABLE                                         */
/**********************************************************************/
#define MEM_CST_MAX_IND     USHRT_MAX //TwoBytes DF_CstTabInd (dataformats.c)

extern MemPtr MEM_cstBase;            //starting address of count sym tab

void  MEM_cstInit(MemPtr base);           //initialize const symbol table
void  MEM_cstSimInit(MemPtr base);        //initialize cst for simulator

                                          //add one entry at the current top
void  MEM_cstInsert(char* name, int teSize, DF_Type tySk, int uc, int pred, 
                    int fixity);

char*   MEM_cstName(int n);              //name of the const in the nth entry 
int     MEM_cstTyEnvSize(int n);         //type env size in the nth entry
int     MEM_cstUnivCount(int n);         //universe count in the nth entry
int     MEM_cstPred(int n);              //precedence of the const in nth entry
int     MEM_cstFixity(int n);            //fixity of the const in nth entry
DF_Type MEM_cstTySkel(int n);            //type skeleton of const in nth entry



/*********************************************************************/
/*      STREAM SYMBOL TABLE                                          */
/*********************************************************************/


#endif  //MEMORY_H
