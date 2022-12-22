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

WordPtr LD_SEARCHTAB_LoadHashTab(MEM_GmtEnt* ent, int* size,int query)
{
  int i;
  int cst;
  struct HashTabEnt *tabEnt;
  struct HashTabEnt *tab;
  int numEntries=*size=LD_FILE_GET2();
  LD_debug("Hash table has %d entries\n",numEntries);
  tab=(struct HashTabEnt*)
	LD_LOADER_ExtendModSpaceInByte(ent,sizeof(struct HashTabEnt)*numEntries);
  for(i=0;i<numEntries;i++)
  {
    tab[i].constInd=-1;
    tab[i].next=NULL;
  }
  
  tabEnt=NULL;
  for(i=0;i<numEntries;i++)
  {
	cst=(int)LD_CONST_GetConstIndQuery(query);
    tabEnt=&(tab[Hash(cst,numEntries)]);
    if(tabEnt->constInd==-1)
    {
      tabEnt->constInd=cst;
      tabEnt->codeAddr=LD_CODE_GetCodeInd();
    } else {
      while(tabEnt->next!=NULL)
        tabEnt=tabEnt->next;
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
  struct HashTabEnt *tabEnt=&(((struct HashTabEnt*)(STabAddr))[Hash(constInd,STabSize)]);
  struct HashTabEnt *tmp;
  tmp = tabEnt;

  //  do
  while(tabEnt != NULL)
  {
      if(tabEnt->constInd==constInd){
	//fprintf(stderr, "found: %u\n", tabEnt -> codeAddr);
          return tabEnt->codeAddr;
      }
      tabEnt=tabEnt->next;
  }
  //while(tabEnt->next!=NULL);
  //constInd not found
  return NULL;
}

typedef struct
{
  int constInd;
  CSpacePtr codeAddr;
} SeqSTabEnt;

WordPtr LD_SEARCHTAB_LoadSeqSTab(MEM_GmtEnt* ent, int* size, int query)
{
  int numEntries=*size=LD_FILE_GET2();
  int i;

  SeqSTabEnt* tab=(SeqSTabEnt*)LD_LOADER_ExtendModSpaceInByte(ent,sizeof(SeqSTabEnt)*numEntries);
  for(i=0;i<numEntries;i++)
  {
	tab[i].constInd=(int)LD_CONST_GetConstIndQuery(query);
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
