#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "code.h"
#include "const.h"
#include "../system/memory.h"
#include "searchtab.h"

TwoBytes LD_IMPORTTAB_numImportTabs;
WordPtr* LD_IMPORTTAB_ImportTabs;

WordPtr LD_IMPORTTAB_LoadImportTab(MEM_GmtEnt* ent);

void LD_IMPORTTAB_LoadImportTabs(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_IMPORTTAB_numImportTabs=LD_FILE_GET2();
  LD_IMPORTTAB_ImportTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  ent->addtable=(CSpacePtr)LD_IMPORTTAB_LoadImportTab(ent);
  for(i=0;i<count;i++)
  {
    LD_IMPORTTAB_ImportTabs[i]=LD_IMPORTTAB_LoadImportTab(ent);
  }
  
  return;
}

WordPtr LD_IMPORTTAB_LoadImportTab(MEM_GmtEnt* ent)
{
  ///\todo Check on the space requirements of the import table.
  WordPtr tab=LD_LOADER_ExtendModSpace(ent,(3)*sizeof(Word));
  int cst;
  int i;
  int numSegs = LD_FILE_GET2();//Get number of segments.
  MEM_impPutNCSEG(tab,numSegs);
  //Load Next Clause Table
  int nctSize=(int)LD_FILE_GET2();
  MEM_impPutLTS(tab,nctSize);
  for(i=0;i<nctSize;i++)
  {
    cst=(int)LD_CONST_GetConstInd();
    MEM_impPutLT(tab,i,cst);
  }
  
  //Local Constant table
  int lctSize=(int)LD_FILE_GET2();
  MEM_impPutNLC(tab, lctSize);
  MemPtr lcTab=MEM_impLCT(tab,nctSize);///\todo Reorder Link table and local constant table
  for(i=0;i<nctSize;i++)
  {
    cst=(int)LD_CONST_GetConstInd();
    MEM_impPutLCT(tab,i,cst);
  }
  
  //Load FindCodeFunc
  Byte fcf=LD_FILE_GET1();
  int psts;
  if(fcf==SEARCHTAB_FCF_SEQNSEARCH)
  {
    MEM_impPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_SeqnSrch);
    LD_SEARCHTAB_LoadSeqSTab(ent,&psts);
    MEM_impPutPSTS(tab,psts);
    ///\todo do something with returned address.
  }
  else if(fcf==SEARCHTAB_FCF_HASHSEARCH)
  {
    MEM_impPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_HashSrch);
    LD_SEARCHTAB_LoadHashTab(ent,&psts);
    MEM_impPutPSTS(tab,psts);
    ///\todo do something with returned address.
  }
  return tab;
}

WordPtr LD_IMPORTTAB_GetImportTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_IMPORTTAB_numImportTabs)
    EM_THROW(LD_LoadError);
  return LD_IMPORTTAB_ImportTabs[i];
}
