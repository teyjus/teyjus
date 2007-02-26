#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "code.h"
#include "const.h"
#include "../system/memory.h"

void LD_ADDCODE_LoadModAddCodeTab(MEM_GmtEnt* ent)
{
  ent->addtable=LD_ADDCODE_LoadAddCodeTab(ent);
  
  return;
}

CSpacePtr LD_ADDCODE_LoadAddCodeTab(MEM_GmtEnt* ent)
{
  //Load FindCodeFunc
  Byte fcf=LD_FILE_GET1();
  ///\todo do something sensible with the find code function.
  //MEM_implPutFCP(tab,(MEM_FindCodeFnPtr)NULL);//TODO
  
  //Load Search table
  Word sTabSize=(Word)LD_FILE_GET2();
  
  ///\todo Make search tables use hashes
  Word* sTab=LD_LOADER_ExtendModSpace(ent,2*sizeof(Word)*sTabSize);
  for(int i=0;i<nop;i++)
  {
    sTab[2*i]=(Word)LD_CONST_GetConstInd();
    sTab[2*i+1]=(Word)LD_CODE_GetCodeInd();
  }
  
  return sTab;
}

WordPtr LD_IMPLGOAL_GetImplGoal()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_LOADER_numImplGoals)
    EM_THROW(LD_LoadError);
  return LD_LOADER_ImplGoals[i];
}
