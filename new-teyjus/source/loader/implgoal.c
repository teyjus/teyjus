#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "code.h"
#include "const.h"
#include "../system/memory.h"
#include "addcode.h"

TwoBytes LD_IMPLGOAL_numImplGoals;
WordPtr* LD_IMPLGOAL_ImplGoals;

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent);

void LD_IMPLGOAL_LoadImplGoals(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_IMPLGOAL_numImplGoals=LD_FILE_GET2();
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
  
  WordPtr tab=LD_LOADER_ExtendModSpace(ent,(3+nctSize)*sizeof(Word));
  
  Word cst;
  MEM_implPutNOP(tab,(Word)nctSize);
  for(i=0;i<nctSize;i++)
  {
    cst=(Word)LD_CONST_GetConstInd();
    MEM_implPutLT(tab,i,cst);
  }
  
  //Load FindCodeFunc
  //Byte fcf=LD_FILE_GET1();
  //MEM_implPutFCP(tab,(MEM_FindCodeFnPtr)NULL);
  ///\todo fix Impl goal loading
  //LD_ADDCODE_LoadAddCodeTab(ent);
  
  return tab;
}

WordPtr LD_IMPLGOAL_GetImplGoalAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_IMPLGOAL_numImplGoals)
    EM_THROW(LD_LoadError);
  return LD_IMPLGOAL_ImplGoals[i];
}
