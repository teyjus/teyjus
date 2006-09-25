#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "../system/memory.h"

TwoBytes LD_IMPLGOAL_numImplGoals;
WordPtr* LD_IMPLGOAL_ImplGoals;

void LD_IMPLGOAL_LoadImplGoals(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_LOADER_numImplGoals=LD_FILE_GET2();
  LD_LOADER_ImplGoals=(WordPtr*)malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {
    LD_LOADER_ImplGoals[i]=LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent);
  }
  
  return;
}

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent)
{
  int i;
  int nctSize=(int)LD_FILE_GET2();
  
  WordPtr tab=LD_LOADER_ExtendModSpace(ent,(3+nop)*sizeof(Word));
  
  Word cst;
  MEM_implPutNOP(tab,(INT4)nop);
  for(i=0;i<nop;i++)
  {
    cst=(Word)LD_CONST_GetConstInd();
    MEM_implPutLT(tab,i,cst);
  }
  
  Word fcf=(Word)LD_FILE_GET1();
  MEM_implPutFCP(tab,fcf);
  
  INT4 searchtabsize;
  INT4 nop;
  nop=searchtabsize=(INT4)LD_FILE_GET2();
  
  //TODO Make search tables use hashes
  Word* sTab=LD_LOADER_ExtendModSpace(ent,2*sizeof(Word)*nop);
  for(int i=0;i<nop;i++)
  {
    sTab[2*i]=(Word)LD_CONST_GetConstInd();
    sTab[2*i+1]=(Word)LD_CODE_GetCodeInd();
  }
  
  return tab;
}

WordPtr LD_IMPLGOAL_GetImplGoal()
{
  int i =(int) LD_FILE_GET2();
  if(0<=i && i<= LD_LOADER_numImplGoals)
  {
    return LD_LOADER_ImplGoals[i];
  }
  else
    return NULL; //TODO Throw exception?
}
