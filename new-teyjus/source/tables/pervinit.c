/***************************************************************************/
/* File pervinit.h{c}.                                                     */
/* Functions for setting up the symbol tables of pervasive constants and   */
/* kinds are provided.                                                     */
/***************************************************************************/
#include <string.h>

#include "pervinit.h"
#include "pervasives.h"
#include "../simulator/dataformats.h"
#include "../simulator/mcstring.h"
#include "../simulator/mctypes.h"

DF_StrDataPtr PERVINIT_writeName(DF_StrDataPtr loc, char* name)
{
    int    length  = strlen(name);
    int    size    = MCSTR_numWords(length);
    MemPtr mcStr   = ((MemPtr)loc) + DF_STRDATA_HEAD_SIZE;
    MemPtr nextLoc = mcStr + size;
    
    //should add overflow checking; increase modSpaceEnd?

    //create data head
    DF_mkStrDataHead((MemPtr)loc);
    //create string
    MCSTR_toString((MCSTR_Str)mcStr, name, length);
    
    return ((DF_StrDataPtr)nextLoc);
}


/***************************************************************************/
/* Set up pervasive kind symbol table.                                     */
/* The kind names are supposed to be written in the string area starting   */
/* from the address given by strLoc. The next available slot in the string */
/* area after loading these names are returned.                            */ 
/***************************************************************************/
DF_StrDataPtr PERVINIT_kindTabInit(MEM_KstPtr tabLoc, DF_StrDataPtr strLoc)
{
    int tabInd;

    for (tabInd = 0; tabInd < PERV_KIND_NUM; tabInd++) {
        tabLoc[tabInd].name = strLoc;
        tabLoc[tabInd].arity = PERV_kindDataTab[tabInd].arity;
        strLoc = PERVINIT_writeName(strLoc, PERV_kindDataTab[tabInd].name);
    }
    return strLoc;
}

/***************************************************************************/
/* Set up pervasive constant symbol table.                                 */
/* The constant names are supposed to be written in the string area        */
/* starting from the address given by strLoc. The next available slot in   */
/* the string area after loading these names are returned.                 */  
/***************************************************************************/
DF_StrDataPtr PERVINIT_constTabInit(MEM_CstPtr tabLoc, DF_StrDataPtr strLoc)
{
    int tabInd;
    
    for (tabInd = 0; tabInd < PERV_CONST_NUM; tabInd++) {
        tabLoc[tabInd].name = strLoc;
        tabLoc[tabInd].typeEnvSize = PERV_constDataTab[tabInd].typeEnvSize;
        tabLoc[tabInd].tskTabIndex = PERV_constDataTab[tabInd].tskTabIndex;
        tabLoc[tabInd].neededness = PERV_constDataTab[tabInd].neededness;
        tabLoc[tabInd].univCount = PERV_constDataTab[tabInd].univCount;
        tabLoc[tabInd].precedence = PERV_constDataTab[tabInd].precedence;
        tabLoc[tabInd].fixity = PERV_constDataTab[tabInd].fixity;
        strLoc = PERVINIT_writeName(strLoc, PERV_constDataTab[tabInd].name);
    }
    return strLoc;
}

