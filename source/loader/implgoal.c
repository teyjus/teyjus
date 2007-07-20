#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "code.h"
#include "const.h"
#include "../system/memory.h"
#include "searchtab.h"
#include "ld_message.h"

TwoBytes LD_IMPLGOAL_numImplGoals;
WordPtr* LD_IMPLGOAL_ImplGoals;

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent);

void LD_IMPLGOAL_LoadImplGoals(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_IMPLGOAL_numImplGoals=LD_FILE_GET2();
  LD_detail("Loading %d implication goals\n",count);
  LD_IMPLGOAL_ImplGoals=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {
    LD_IMPLGOAL_ImplGoals[i]=LD_IMPLGOAL_LoadImplGoal(ent);
  }
  
  return;
}

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent)
{
  int i;
  
  //Load Next Clause Table
  int nctSize=(int)LD_FILE_GET2();
  ///\todo Check on the space requirements of the implgoal table.
  WordPtr tab=LD_LOADER_ExtendModSpace(ent,(3+nctSize)*sizeof(Word));
  
  int cst;
  MEM_implPutLTS(tab,nctSize);
  for(i=0;i<nctSize;i++)
  {
    cst=(int)LD_CONST_GetConstInd();
    MEM_implPutLT(tab,i,cst);
  }
  
  //Load FindCodeFunc
#define FCF_SEQNSEARCH 1
#define FCF_HASHSEARCH 2
///\todo Correct and move these: shared with addcode.c
  Byte fcf=LD_FILE_GET1();
  int tabSize;
  if(fcf==FCF_SEQNSEARCH)
  {
    MEM_implPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_SeqnSrch);
    LD_SEARCHTAB_LoadSeqSTab(ent,&tabSize);
    MEM_implPutPSTS(tab,tabSize);
    ///\todo do something with returned address.
  }
  else if(fcf==FCF_HASHSEARCH)
  {
    MEM_implPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_HashSrch);
    LD_SEARCHTAB_LoadHashTab(ent,&tabSize);
    MEM_implPutPSTS(tab,tabSize);
    ///\todo do something with returned address.
  }
  return tab;
}

WordPtr LD_IMPLGOAL_GetImplGoalAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_IMPLGOAL_numImplGoals)
    EM_THROW(LD_LoadError);
  return LD_IMPLGOAL_ImplGoals[i];
}
