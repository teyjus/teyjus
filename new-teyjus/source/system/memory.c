#ifndef MEMORY_C
#define MEMORY_C

#include "error.h"
#include "memory.h"
#include "../simulator/mctypes.h" //to be changed

/******************************************************************************/
/*                SYSTEM MEMORY MANAGEMENT  (TEMP)                            */
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

/*****************************************************************************/
/*               ACCESSING THE IMPLICATION GOAL TABLE                        */
/*****************************************************************************/
/* functions for filling in the fields of a impl table                       */
/* Q: the data stored in each field in byte code: are they word or in their  */
/*    specific types?                                                        */
//#pred field (def extended)
void MEM_implPutLTS(WordPtr tab, Word nop)        { *tab = nop;             }
//ptr to find code func
void MEM_implPutFC(WordPtr tab, Word fcPtr)       { *(tab + 1) = fcPtr;     }
//num entries in the link tab
void MEM_implPutPSTS(WordPtr tab, Word tabSize)   { *(tab + 2) = tabSize;   }
//fill in the ith entry of the link tab
//Note: index should start from 0
void MEM_implPutLT(WordPtr tab, int ind, Word cst){ *(tab + 3 + ind) = cst; }

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
void    MEM_impPutNCSEG(WordPtr tab, Word nseg)      {   *tab = nseg;          }
//# local constants
void    MEM_impPutNLC(WordPtr tab, Word nlc)         {   *(tab + 1) = nlc;     }
//# pred (def extended)
void    MEM_impPutLTS(WordPtr tab, Word nop)         {   *(tab + 2) = nop;     }
//ptr to find code func
void    MEM_impPutFC(WordPtr tab, Word fcp)          {   *(tab + 3) = fcp;     }
//# entries in link tab
void    MEM_impPutPSTS(WordPtr tab, Word tabSize)    {   *(tab + 4) = tabSize; }
//link tab 
//Note: ind should start from 0
void    MEM_impPutLT(WordPtr tab, int ind, Word pred){   *(tab+5+ind) = pred;  }
//loc c tab(may null)
//Note 1) the input tab addr should be the starting addr of the local const tab
//     2) ind should start from 0
void    MEM_impPutLCT(WordPtr lcTab, int ind, Word cst){ *(lcTab+ind) = cst; }

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
/*    ACCESSING THE BOUND VARIABLE INDEXING TABLE (BRACHING TABLE)           */
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

#endif  //MEMORY_C
