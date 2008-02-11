#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "searchtab.h"
#include "ld_message.h"

TwoBytes LD_HASHTAB_numHashTabs;
WordPtr* LD_HASHTAB_HashTabs;

void LD_HASHTAB_LoadHashTabs(MEM_GmtEnt* ent)
{
  int i;
  int ignore;///\note We do not check if the size given for a hash table matches the size used in code.
  TwoBytes count=LD_HASHTAB_numHashTabs=LD_FILE_GET2();
  LD_detail("Loading %d hash tables\n",count);
  LD_HASHTAB_HashTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {   
    LD_HASHTAB_HashTabs[i]=LD_SEARCHTAB_LoadHashTab(ent,&ignore);
  }
  return;
}

WordPtr LD_HASHTAB_GetHashTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_HASHTAB_numHashTabs)
    EM_THROW(LD_LoadError);
  return LD_HASHTAB_HashTabs[i];
}
