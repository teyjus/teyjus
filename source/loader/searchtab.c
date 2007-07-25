#include "file.h"
#include "loader.h"
#include "const.h"
#include "code.h"
#include "searchtab.h"
#include "../system/memory.h"
#include "ld_message.h"

struct HashTabEnt;

struct HashTabEnt
{
  int constInd;
  CSpacePtr codeAddr;
  struct HashTabEnt* next;
};


int Hash(int ci,int size)
{
  return ci%size;
}

WordPtr LD_SEARCHTAB_LoadHashTab(MEM_GmtEnt* ent, int* size)
{
  int numEntries=*size=LD_FILE_GET2();
  LD_debug("Hash table has %d entries\n",numEntries);
  int i;
  /*
  struct HashTabEnt *tab=(struct HashTabEnt*)LD_LOADER_ExtendModSpace(ent,sizeof(struct HashTabEnt)*numEntries); --XQ
  */
  struct HashTabEnt *tab=(struct HashTabEnt*)LD_LOADER_ExtendModSpaceInByte(ent,sizeof(struct HashTabEnt)*numEntries);
  for(i=0;i<numEntries;i++)
  {
    tab[i].constInd=-1;
    tab[i].next=NULL;
  }
  
  int cst;
  struct HashTabEnt *tabEnt=NULL;
  for(i=0;i<numEntries;i++)
  {
    cst=(int)LD_CONST_GetConstInd();
    tabEnt=&(tab[Hash(cst,numEntries)]);
    if(tabEnt->constInd==-1)
    {
      tabEnt->constInd=cst;
      tabEnt->codeAddr=LD_CODE_GetCodeInd();
    } else {
      while(tabEnt->next!=NULL)
        tabEnt=tabEnt->next;
      /*
      tabEnt=tabEnt->next=(struct HashTabEnt*)LD_LOADER_ExtendModSpace(ent,sizeof(struct HashTabEnt));
      -- XQ */
      tabEnt=tabEnt->next=
          (struct HashTabEnt*)LD_LOADER_ExtendModSpaceInByte(ent,sizeof(struct HashTabEnt));
      tabEnt->constInd=cst;
      tabEnt->codeAddr=LD_CODE_GetCodeInd();
      tabEnt->next=NULL;
    }
  }
  return (WordPtr)tab;
}

CSpacePtr LD_SEARCHTAB_HashSrch(int constInd, int STabSize, MemPtr STabAddr)
{
  int i;  
  struct HashTabEnt *tabEnt=&(((struct HashTabEnt*)(STabAddr))[Hash(constInd,STabSize)]);
  struct HashTabEnt *tmp;
  tmp = tabEnt;

  do
  {
      if(tabEnt->constInd==constInd){
          return tabEnt->codeAddr;
      }
      tabEnt=tabEnt->next;
  }while(tabEnt->next!=NULL);
  //constInd not found
  return NULL;
}

typedef struct
{
  int constInd;
  CSpacePtr codeAddr;
} SeqSTabEnt;

WordPtr LD_SEARCHTAB_LoadSeqSTab(MEM_GmtEnt* ent, int* size)
{
  int numEntries=*size=LD_FILE_GET2();
  int i;

  //SeqSTabEnt* tab=(SeqSTabEnt*)LD_LOADER_ExtendModSpace(ent,sizeof(SeqSTabEnt)*numEntries); -- XQ
  SeqSTabEnt* tab=(SeqSTabEnt*)LD_LOADER_ExtendModSpaceInByte(ent,sizeof(SeqSTabEnt)*numEntries);
  for(i=0;i<numEntries;i++)
  {
    tab[i].constInd=(int)LD_CONST_GetConstInd();
    tab[i].codeAddr=LD_CODE_GetCodeInd();
  }
  
  return (WordPtr)tab;
}

CSpacePtr LD_SEARCHTAB_SeqnSrch(int constInd, int STabSize, MemPtr STabAddr)
{
  int i;
  SeqSTabEnt* tab=(SeqSTabEnt*)STabAddr;
  for(i=0;i<STabSize;i++)
  {
    if(tab[i].constInd==constInd)
      return tab[i].codeAddr;
  }
  //constInd not found
  return NULL;
}
