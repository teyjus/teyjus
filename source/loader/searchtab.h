#ifndef _SEARCHTAB_H_
#define _SEARCHTAB_H_

#include "../system/memory.h"

#define SEARCHTAB_FCF_SEQNSEARCH 0
#define SEARCHTAB_FCF_HASHSEARCH 1

extern WordPtr LD_SEARCHTAB_LoadHashTab(MEM_GmtEnt* ent, int* size);

extern WordPtr LD_SEARCHTAB_LoadSeqSTab(MEM_GmtEnt* ent, int* size);

/**
\brief Find code function for hash tables.
\return The address of the code corresponding to the given index.  Return address for values not in the table is undefined.
**/
extern CSpacePtr LD_SEARCHTAB_HashSrch(int constInd, int STabSize, MemPtr STabAddr);

/**
\brief Find code function for sequential search tables.
\return The address of the code corresponding to the given index.  Return address for values not in the table is undefined.
 **/
extern CSpacePtr LD_SEARCHTAB_SeqnSrch(int constInd, int STabSize, MemPtr STabAddr);

#endif //_SEARCHTAB_H_
