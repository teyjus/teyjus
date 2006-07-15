/****************************************************************************/
/*                                                                          */
/*                                                                          */
/*   File  memory.h. This header file defines the structures that are used  */
/*   by the simulator, the loader and the top-level interation system.      */
/*                                                                          */
/****************************************************************************/

#ifndef MEMORY_H
#define MEMORY_H

#include <limits.h>
#include <math.h>
#include "../simulator/mctypes.h"        //to be changed
#include "../simulator/dataformats.h"    //to be changed
//#include "../config.h"

/******************************************************************************/
/*                SYSTEM MEMORY MANAGEMENT  (TEMP)                            */
/******************************************************************************/
extern  WordPtr MEM_memBeg;       //starting addr of the system memory
extern  WordPtr MEM_memEnd;       //end addr of the system memory
extern  WordPtr MEM_memTop;       //the first usable word in the system memory
extern  WordPtr MEM_memBot;       //the last usable word in the system memory


/* Asking for the system memory of a given size (in word),                    */
/* and initialize relevant global variables.                                  */
void    MEM_memInit(unsigned int size);

/*****************************************************************************/
/*                          KIND SYMBOL TABLE                                */
/*****************************************************************************/
/*  kind symbol table entry                                                  */
typedef struct                                       
{
    char      *name;
    TwoBytes  arity; //agree with DF_StrTypeArity (simulator/dataformats.c)
} MEM_KstEnt;

typedef MEM_KstEnt *MEM_KstPtr;

/*  max possible index of kind table                                        */
/*  (agree with DF_KstTabInd in simulator/dataformats.c)                    */ 
#define MEM_KST_MAX_IND        USHRT_MAX

/*  size of each entry of this table (in word)                              */
//Note this arithematic should in reality go into "config.h"
#define MEM_KST_ENTRY_SIZE     (int)ceil(sizeof(MEM_KstEnt)/WORD_SIZE)

/*****************************************************************************/
/*                         TYPE SKELETON TABLE                               */
/*****************************************************************************/
/* type skeleton table entry                                                 */
typedef DF_TypePtr MEM_TstEnt;

typedef MEM_TstEnt *MEM_TstPtr;

/*  max possible index of type skeleton table                                */
#define MEM_TST_MAX_IND        USHRT_MAX 

/*  size of each entry of this table (in word)                               */
//Note this arithematic should in reality go into "config.h"
#define MEM_TST_ENTRY_SIZE    (int)ceil(sizeof(MEM_TstEnt)/WORD_SIZE)

/*****************************************************************************/
/*                        CONSTANT SYMBOL TABLE                              */
/*****************************************************************************/
/*  constant symbol table entry                                              */ 
typedef struct
{
    char      *name;
    TwoBytes  typeEnvSize;
    TwoBytes  tskTabIndex;     //index to the type skeleton table 
    //word      neednessVec;   //needness vector (predicate constant)
    TwoBytes  univCount;
    int       precedence;
    int       fixity;
} MEM_CstEnt;

typedef MEM_CstEnt *MEM_CstPtr;

/*  max possible index of constant symbol table                             */
/*  (agree with DF_CstTabInd in simulator/dataformats.c)                    */
#define MEM_CST_MAX_IND     USHRT_MAX
                                          //add one entry at the current top
/*  size of each entry of this table (in word)                               */
//Note this arithematic should in reality go into "config.h"
#define MEM_CST_ENTRY_SIZE    (int)(sizeof(MEM_CstEnt)/WORD_SIZE)


/*****************************************************************************/
/*                          GLOBAL MODULE TABLE                              */
/*****************************************************************************/
typedef struct
{
    char        *modname;        //(top-level) module name 
    CSpacePtr   addtable;        //addr to the add code table of the top module
    MEM_KstPtr  kstBase;         //starting addr of kind table
    MEM_TstPtr  tstBase;         //starting addr of type skel table
    MEM_CstPtr  cstBase;         //starting addr of constant table
    WordPtr     modSpaceBeg;     //starting addr of module space
    WordPtr     modSpaceEnd;     //ending addr of module space
	WordPtr		codeSpaceBeg;    //starting addr of module space
	WordPtr		codeSpaceEnd;    //ending addr of module space
} MEM_GmtEnt;

#define MEM_MAX_MODULES    255   //max number of modules (temp)

typedef MEM_GmtEnt MEM_Gmt[MEM_MAX_MODULES];

extern  MEM_Gmt    MEM_modTable; //global module table 


/*****************************************************************************/
/*               ACCESSING THE IMPLICATION GOAL TABLE                        */
/*****************************************************************************/
/* functions for filling in the fields of an impl table                      */
void    MEM_implPutNOP(WordPtr tab, Word nop);        //# pred (def extended)
void    MEM_implPutFCP(WordPtr tab, Word fcPtr);      //ptr to find code func
void    MEM_implPutTS(WordPtr tab, Word tabSize);     //# entries link tab
void    MEM_implPutLT(WordPtr tab, int ind, Word cst);//link tab
//Note the tab addr is calculated from the start of the impl tab
WordPtr MEM_implMovToPT(WordPtr tab, int nop);        //start addr of search tab

/* functions for retrieving the fields of a impl table                       */
Mem    MEM_implNOP(MemPtr tab);  //pred field (def extended)
MemPtr MEM_implFCP(MemPtr tab);  //ptr to find code func
Mem    MEM_implTS(MemPtr tab);   //num entries in link tab
MemPtr MEM_implLT(MemPtr tab);   //start add of seq. of pred (link tab)
MemPtr MEM_implPT(MemPtr tab);   //start add of search tab



/*****************************************************************************
 *                  ACCESSING THE IMPORTED MODULE TABLE                      *
 *****************************************************************************/
/* functions for filling in the fields of an import table                   */
void    MEM_impPutCSEG(WordPtr tab, Word nseg); //# code segments
void    MEM_impPutNLC(WordPtr tab, Word nlc);   //# local constants
void    MEM_impPutNOP(WordPtr tab, Word nop);   //# pred (def extended)
void    MEM_impPutFCP(WordPtr tab, Word fcp);   //ptr to find code func
void    MEM_impPutTS(WordPtr tab, Word tabSize);//# entries in link tab
void    MEM_impPutLT(WordPtr tab, int ind, Word pred);  //link tab
//Note the input tab addr should be the starting addr of the local const tab
void    MEM_impPutLCT(WordPtr lcTab, int ind, Word cst);//loc c tab(may null)
//Note the tab addr is calculated from the start of the imp tab
WordPtr MEM_impMovToLCT(WordPtr tab, int nop);  //start addr of local c tab
//Note the tab addr is calculated from the start of the imp tab
WordPtr MEM_impMovToPT(WordPtr tab, int nloc, int nop);//start addr of sch tab 


/* functions for retrieving the fields of a impl table                       */
Mem    MEM_impCSEG(MemPtr tab); //# code segments
Mem    MEM_impNLC(MemPtr tab);  //# local constants
Mem    MEM_impNOP(MemPtr tab);  //# of preds (def extended)
MemPtr MEM_impFCP(MemPtr tab);  //ptr to find code func
Mem    MEM_impTS(MemPtr tab);   //# entries in link tab
MemPtr MEM_impLT(MemPtr tab);   //start addr of seq. of pred (link tab)
MemPtr MEM_impLCT(MemPtr tab);  //start addr of local const tab (possible null)
MemPtr MEM_impPT(MemPtr tab);   //start addr of search tab


/*****************************************************************************
 *                      functions for finding code                           *
 *****************************************************************************/



#endif  //MEMORY_H
