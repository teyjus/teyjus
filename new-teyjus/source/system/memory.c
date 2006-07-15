#ifndef MEMORY_C
#define MEMORY_C

#include "error.h"
#include "memory.h"
#include "../simulator/mctypes.h" //to be changed

/*****************************************************************************/
/*                          GLOBAL MODULE TABLE                              */
/*****************************************************************************/
MEM_Gmt    MEM_modTable;                 //global module table 

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
//#pred field (def extended)
void MEM_implPutNOP(WordPtr tab, Word nop)        { *tab = nop;             }
//ptr to find code func
void MEM_implPutFCP(WordPtr tab, Word fcPtr)      { *(tab + 1) = fcPtr;     }
//num entries in the link tab
void MEM_implPutTS(WordPtr tab, Word tabSize)     { *(tab + 2) = tabSize;   }
//link tab
void MEM_implPutLT(WordPtr tab, int ind, Word cst){ *(tab + 3 + ind) = cst; }
//start addr of search tab
WordPtr MEM_implMovPT(WordPtr tab, int nop)       { return (tab + 3 + nop); }


/* functions for retrieving the fields of a impl table                       */
//pred field (def extended)
Mem    MEM_implNOP(MemPtr tab){ return (*tab);              }
//ptr to find code func
MemPtr MEM_implFCP(MemPtr tab){ return (MemPtr)(*(tab + 1));}
//num entries in the link tab
Mem    MEM_implTS(MemPtr tab) { return (*(tab + 2));        }
//start add of seq. of pred (link tab)
MemPtr MEM_implLT(MemPtr tab) { return (MemPtr)(tab + 3);   }
//start add of search tab
MemPtr MEM_implPT(MemPtr tab)    
{
    //tab+3+ MEM_implNOP(tab) ?
    return (MEM_implLT(tab)+ (long)MEM_implNOP(tab));
}

/*****************************************************************************
 *                  ACCESSING THE IMPORTED MODULE TABLE                      *
 *****************************************************************************/
/* functions for filling in the fields of an import table                   */
//# code segments
void    MEM_impPutCSEG(WordPtr tab, Word nseg)     {   *tab = nseg;          }
//# local constants
void    MEM_impPutNLC(WordPtr tab, Word nlc)       {   *(tab + 1) = nlc;     }
//# pred (def extended)
void    MEM_impPutNOP(WordPtr tab, Word nop)       {   *(tab + 2) = nop;     }
//ptr to find code func
void    MEM_impPutFCP(WordPtr tab, Word fcp)       {   *(tab + 3) = fcp;     } 
//# entries in link tab
void    MEM_impPutTS(WordPtr tab, Word tabSize)    {   *(tab + 4) = tabSize; }
//link tab
void    MEM_impPutLT(WordPtr tab, int ind, Word pred){ *(tab+5+ind) = pred;  }
//start addr of local c tab    
//Note the tab addr is calculated from the start of the imp tab
WordPtr MEM_impMovToLCT(WordPtr tab, int nop)      {  return (tab + 5 + nop);}  
//loc c tab(may null)
//Note the input tab addr should be the starting addr of the local const tab
void    MEM_impPutLCT(WordPtr lcTab, int ind, Word cst){ *(lcTab+ind) = cst; }
//start addr of sch tab
//Note the tab addr is calculated from the start of the imp tab
WordPtr MEM_impMovToPT(WordPtr tab, int nloc, int nop){return (tab+5+nloc+nop);}


/* functions for retrieving the fields of a impl table                       */
//# code segments
Mem    MEM_impCSEG(MemPtr tab)   {  return *tab;               }
//# local constants 
Mem    MEM_impNLC(MemPtr tab)    {  return *(tab+1);           }  
//# of preds (def extended)
Mem    MEM_impNOP(MemPtr tab)    {  return *(tab+2);           }  
//ptr to find code func
MemPtr MEM_impFCP(MemPtr tab)    {  return (MemPtr)(*(tab+3)); } 
//# entries in pred name tab 
Mem    MEM_impTS(MemPtr tab)     {  return *(tab+4);           }  
//start addr of seq. of pred (pred name tab) 
MemPtr MEM_impLT(MemPtr tab)     {  return (MemPtr)(*(tab+5)); } 
//start addr of local const tab (possible null)
MemPtr MEM_impLCT(MemPtr tab) 
{
    return (MEM_impLT(tab) + (long)MEM_impNOP(tab));//tab + 5 + MEM_impNOP(tab)?
}
//start addr of search tab
MemPtr MEM_impPT(MemPtr tab) 
{
    return (MEM_impLCT(tab) + (long)MEM_impNLC(tab));
}

#endif  //MEMORY_C
