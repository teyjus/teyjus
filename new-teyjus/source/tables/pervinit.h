/***************************************************************************/
/* File pervinit.h{c}.                                                     */
/* Functions for setting up the symbol tables of pervasive constants and   */
/* kinds are provided.                                                     */
/***************************************************************************/

#ifndef PERVINIT_H
#define PERVINIT_H

#include "../simulator/dataformats.h" //to be modified
#include "../system/memory.h"         //to be modified
#include "pervasives.h"
#include "../simulator/mctypes.h"

/***************************************************************************/
/*                   PERVASIVE KINDS                                       */
/***************************************************************************/
//the actual pervasive kind table get copied during loading
extern MEM_KstEnt PERVINIT_kindDataTab[PERV_KIND_NUM];

/* copy the pervasive kind table into given address                        */
void PERVINIT_copyKindDataTab(MEM_KstPtr dst);


/***************************************************************************/
/*                   PERVASIVE TYPE SKELETONS                              */
/***************************************************************************/
//pervasive type skeleton table
extern MEM_TstPtr PERVINIT_tySkelTab;

/* copy the pervasive type skeleton table into given address               */
void PERVINIT_copyTySkelTab(MEM_TstPtr dst);


/***************************************************************************/
/*                   PERVASIVE CONSTANTS                                   */
/***************************************************************************/
//the acutual pervasive constant table get copied during loading           
extern MEM_CstEnt PERVINIT_constDataTab[PERV_CONST_NUM];

/* copy the pervasive constant table into given address                    */
void PERVINIT_copyConstDataTab(MEM_CstPtr dst);


/***************************************************************************/
/*                PERVASIVE TABLES INITIALIZATION                          */
/* Fill in the actual pervasive tables; create string data needed for names*/
/* onto the current top of the system memory; create the type skeletons in */
/* a malloced space.                                                       */
/***************************************************************************/
void PERVINIT_tableInit();

#endif //PERVINIT_H
