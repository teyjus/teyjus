//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#ifndef MEMORY_C
#define MEMORY_C

#include <string.h>
#include <stdio.h>
#include "error.h"
#include "memory.h"
#include "error.h"
#include "../tables/pervasives.h"
#include "../tables/pervinit.h"
#include "../simulator/mctypes.h"

/******************************************************************************/
/*                SYSTEM MEMORY MANAGEMENT                                    */
/******************************************************************************/
WordPtr MEM_memBeg;       //starting addr of the system memory
WordPtr MEM_memEnd;       //end addr of the system memory
WordPtr MEM_memTop;       //the first usable word in the system memory
WordPtr MEM_memBot;       //the last usable word in the system memory

/* Asking for the system memory of a given size (in word),                    */
/* and initialize relevant global variables.                                  */
void   MEM_memInit(unsigned int size)
{
    MEM_memBeg = MEM_memTop = (WordPtr)EM_malloc(size * sizeof(Word));
    MEM_memEnd = MEM_memBot = MEM_memBeg + (size - 1);
}

/* Asking the simulator (system) memory for space of a given size (in word)  */
WordPtr MEM_memExtend(unsigned int size)
{
    WordPtr rtPtr = MEM_memTop;
    
    if ((MEM_memTop + size) > MEM_memBot) EM_error(EM_OUT_OF_MEMORY);
    else MEM_memTop += size;
    return rtPtr;
}

/*****************************************************************************/
/*               ACCESSING THE IMPLICATION GOAL TABLE                        */
/*****************************************************************************/
/* functions for filling in the fields of a impl table                       */
//#pred field (def extended)
void MEM_implPutLTS(WordPtr tab, int lts)     { *((int *)tab) = lts;          }
//ptr to find code func
void MEM_implPutFC(WordPtr tab, MEM_FindCodeFnPtr fcPtr)
{ 
    *((MEM_FindCodeFnPtr *)(tab + 1)) = fcPtr;
}
//num entries in the link tab
void MEM_implPutPSTS(WordPtr tab, int tabSize){ *((int *)(tab + 2)) = tabSize;}
//fill in the ith entry of the link tab
//Note: index should start from 0
void MEM_implPutLT(WordPtr tab, int ind, int cst)
{ 
    *((int *)(tab + 3 + ind)) = cst; 
}

/* functions for retrieving the addresses of associated tables               */
//start add of seq. of pred (link tab)
MemPtr MEM_implLT(MemPtr tab)           { return (tab + 3);                    }
//start add of search tab
MemPtr MEM_implPST(MemPtr tab, int lts) { return (tab + 3 + lts);              }

/* functions for retrieving the fields of a impl table                       */
//pred field (def extended)
int    MEM_implLTS(MemPtr tab)          { return *((int *)tab);                }
//ptr to find code func
MEM_FindCodeFnPtr MEM_implFC(MemPtr tab){return *((MEM_FindCodeFnPtr*)(tab+1));}
//num entries in the link tab
int    MEM_implPSTS(MemPtr tab)         { return *((int *)(tab + 2));          }
//ith entry in the link table 
//Note: a) given address should be the start addr of the link table
//      b) index starts from 0 
int    MEM_implIthLT(MemPtr lt, int index) { return *((int *)(lt + index));    }

/*****************************************************************************
 *                  ACCESSING THE IMPORTED MODULE TABLE                      *
 *****************************************************************************/
/* functions for filling in the fields of an import table                   */
/* Q: the data stored in each field in byte code: are they word or in their  */
/*    specific types?                                                        */
//# code segments
void    MEM_impPutNCSEG(WordPtr tab, int nseg)  {*((int *)tab) = nseg;         }
//# local constants
void    MEM_impPutNLC(WordPtr tab, int nlc)     {*((int *)(tab + 1)) = nlc;    }
//# pred (def extended)
void    MEM_impPutLTS(WordPtr tab, int lts)     {*((int *)(tab + 2)) = lts;    }
//ptr to find code func
void    MEM_impPutFC(WordPtr tab, MEM_FindCodeFnPtr fcp)          
{   
    *((MEM_FindCodeFnPtr *)(tab + 3)) = fcp;     
}
//# entries in link tab
void    MEM_impPutPSTS(WordPtr tab, int tabSize){*((int *)(tab + 4)) = tabSize;}
//link tab 
//Note: ind should start from 0
void    MEM_impPutLT(WordPtr tab, int ind, int cst)
{
    *((int *)(tab+5+ind)) = cst;  
}
//loc c tab(may null)
//Note 1) the input tab addr should be the starting addr of the local const tab
//     2) ind should start from 0
void    MEM_impPutLCT(WordPtr lcTab, int ind, int cst)
{
    *((int *)(lcTab+ind)) = cst; 
}

/* functions for retrieving the addresses of associated tables               */
//start addr of seq. of pred names (link tab) 
MemPtr MEM_impLT(MemPtr tab)           {  return (tab + 5);                    }
//start addr of local const tab (possible null)
MemPtr MEM_impLCT(MemPtr tab, int lts) {  return (tab + 5 + lts);              }
//start addr of search tab
MemPtr MEM_impPST(MemPtr tab, int lts, int nlc) { return (tab + 5 + lts + nlc);}

/* functions for retrieving the fields of a impl table                       */
//# code segments
int MEM_impNCSEG(MemPtr tab)           {  return *((int *)tab);                }
//# local constants 
int MEM_impNLC(MemPtr tab)             {  return *((int *)(tab+1));            }
//# of preds (def extended)
int MEM_impLTS(MemPtr tab)             {  return *((int *)(tab+2));            }
//ptr to find code func
MEM_FindCodeFnPtr MEM_impFC(MemPtr tab){return *((MEM_FindCodeFnPtr *)(tab+3));}
//# entries in pred name tab 
int MEM_impPSTS(MemPtr tab)            {  return *((int *)(tab+4));            }
//ith entry in the link table 
//Note 1) the input tab addr should be the starting addr of the link tab
//     2) ind should start from 0
int MEM_impIthLT(MemPtr lt, int ind)   { return *((int *)(lt + ind));          }
//ith entry in the local const table 
//Note 1) the input tab addr should be the starting addr of the local const tab
//     2) ind should start from 0
int MEM_impIthLCT(MemPtr lct, int ind) { return *((int *)(lct + ind));         }

/*****************************************************************************/
/*    ACCESSING THE BOUND VARIABLE INDEXING TABLE (BRANCHING TABLE)          */
/*****************************************************************************/
int       MEM_branchTabIndexVal(MemPtr tab, int index) //the nth index value
{
    return *((int *)(tab + 8*index));
    
}

CSpacePtr MEM_branchTabCodePtr(MemPtr tab, int index)  //transfer addr 
{
    return *((CSpacePtr *)(tab + 8*index + 4));
}

/*****************************************************************************/
/*                          GLOBAL MODULE TABLE                              */
/*****************************************************************************/
MEM_Gmt    MEM_modTable;                 //global module table 

MEM_GmtEnt *MEM_findInModTable(char* name)
{
    int  i;
    char *myname;
    for (i = 0; i <= MEM_MAX_MODULES; i++) {
        if (myname = MEM_modTable[i].modname) 
            if (strcmp(myname, name) == 0) return (MEM_modTable + i);
    } 
    // module must have been found: failure must have been caught by the OCaml 
    // find module function
    return NULL;
}

MEM_GmtEnt *MEM_findFreeModTableEntry()
{
    int i;
    for (i = 0; i<= MEM_MAX_MODULES; i++) {
        if (MEM_modTable[i].modname != NULL) return (MEM_modTable + i);
    }
    // impossible to fail: failure must have been caught by the OCaml 
    // find module function
    return NULL;
}

void MEM_removeModTableEntry(char* name)
{
    MEM_GmtEnt *mod;
    mod = MEM_findInModTable(name);
    free(mod -> modname);
    mod -> modname = NULL;
}

    
MEM_GmtEnt MEM_topModule;               //top module
void MEM_topModuleInit()
{   
	MEM_KstPtr kst = (MEM_KstEnt*)EM_malloc(PERV_KIND_NUM * sizeof(MEM_KstEnt));
	MEM_TstPtr tst = (MEM_TstEnt*)EM_malloc(PERV_TY_SKEL_NUM * sizeof(MEM_TstEnt));
	MEM_CstPtr cst = (MEM_CstEnt*)EM_malloc(PERV_CONST_NUM * sizeof(MEM_CstEnt));
	
    PERVINIT_copyKindDataTab(kst);
    PERVINIT_copyTySkelTab(tst);
    PERVINIT_copyConstDataTab(cst);

    MEM_topModule.modname  = "";
    MEM_topModule.addtable = NULL;
    MEM_topModule.kstBase  = kst;
    MEM_topModule.tstBase  = tst;
	MEM_topModule.tstSize  = PERV_TY_SKEL_NUM;
    MEM_topModule.cstBase  = cst;
	MEM_topModule.cstSize  = PERV_CONST_NUM;
    MEM_topModule.modSpaceBeg = MEM_memTop;
    MEM_topModule.modSpaceEnd = MEM_memTop;
    MEM_topModule.codeSpaceBeg = NULL;
    MEM_topModule.codeSpaceEnd = NULL;    
}

MEM_GmtEnt *MEM_currentModule; //current module being used 


#endif  //MEMORY_C
