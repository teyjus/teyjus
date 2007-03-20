/***************************************************************************/
/* File pervinit.h{c}.                                                     */
/* Functions for setting up the symbol tables of pervasive constants and   */
/* kinds are provided.                                                     */
/***************************************************************************/

#ifndef PERVINIT_H
#define PERVINIT_H

#include "../simulator/dataformats.h" //to be modified
#include "../system/memory.h"         //to be modified

/***************************************************************************/
/* Set up pervasive kind symbol table.                                     */
/* The kind names are supposed to be written in the string area starting   */
/* from the address given by strLoc. The next available slot in the string */
/* area after loading these names are returned.                            */ 
/***************************************************************************/
DF_StrDataPtr PERVINIT_kindTabInit(MEM_KstPtr tabLoc, DF_StrDataPtr strLoc);

/***************************************************************************/
/* Set up pervasive constant symbol table.                                 */
/* The constant names are supposed to be written in the string area        */
/* starting from the address given by strLoc. The next available slot in   */
/* the string area after loading these names are returned.                 */  
/***************************************************************************/
DF_StrDataPtr PERVINIT_constTabInit(MEM_CstPtr tabLoc, DF_StrDataPtr strLoc);

#endif //PERVINIT_H
