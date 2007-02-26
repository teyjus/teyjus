#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "const.h"
#include "code.h"

TwoBytes LD_HASHTAB_numHashTabs;
WordPtr* LD_HASHTAB_HashTabs;

WordPtr LD_HASHTAB_LoadHashTab(MEM_GmtEnt* ent);
void LD_HASHTAB_LoadHashTabs(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_HASHTAB_numHashTabs=LD_FILE_GET2();
  LD_HASHTAB_HashTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {
    LD_HASHTAB_HashTabs[i]=LD_HASHTAB_LoadHashTab(ent);
  }
  return;
}

WordPtr LD_HASHTAB_LoadHashTab(MEM_GmtEnt* ent)
{
  int numEntries=LD_FILE_GET2();
  int i;
  
  ///\todo Make this actually a hash table.
  Word* tab=LD_LOADER_ExtendModSpace(ent,2*sizeof(Word)*numEntries);
  for(i=0;i<numEntries;i++)
  {
    tab[2*i]=(Word)LD_CONST_GetConstInd();
    tab[2*i+1]=(Word)LD_CODE_GetCodeInd();
  }
  
  return (WordPtr)tab;
}

WordPtr LD_HASHTAB_GetHashTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_HASHTAB_numHashTabs)
    EM_THROW(LD_LoadError);
  return LD_HASHTAB_HashTabs[i];
}
