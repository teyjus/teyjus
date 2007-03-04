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
#define FCF_SEQNSEARCH 1
#define FCF_HASHSEARCH 2
///\todo Correct and move these: shared with addcode.c
  Byte fcf=LD_FILE_GET1();
  if(fcf==FCF_SEQNSEARCH)
  {
    //MEM_implPutFCP(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_SeqnSrch);
    ///\todo store find code function somewhere
    LD_SEARCHTAB_LoadSeqSTab(ent);
    ///\todo do something with returned address.
  }
  else if(fcf==FCF_HASHSEARCH)
  {
    //MEM_implPutFCP(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_HashSrch);
    ///\todo store find code function somewhere
    LD_SEARCHTAB_LoadHashTab(ent);
    ///\todo do something with returned address.
  }
}

WordPtr LD_IMPLGOAL_GetImplGoal()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_LOADER_numImplGoals)
    EM_THROW(LD_LoadError);
  return LD_LOADER_ImplGoals[i];
}
