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
#include "implgoal.h"
#include "loader.h"
#include "file.h"
#include "code.h"
#include "const.h"
#include "../system/memory.h"
#include "searchtab.h"
#include "ld_message.h"

#include "../simulator/abstmachine.h"

TwoBytes LD_IMPLGOAL_numImplGoals;
WordPtr* LD_IMPLGOAL_ImplGoals;

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent, int query);

void LD_IMPLGOAL_LoadImplGoals(MEM_GmtEnt* ent, int query)
{
  int i;
  TwoBytes count=LD_IMPLGOAL_numImplGoals=LD_FILE_GET2();
  LD_detail("Loading %d implication goals\n",count);
  LD_IMPLGOAL_ImplGoals=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {
    LD_IMPLGOAL_ImplGoals[i]=LD_IMPLGOAL_LoadImplGoal(ent, query);
  }
  
  return;
}

WordPtr LD_IMPLGOAL_LoadImplGoal(MEM_GmtEnt* ent, int query)
{
  int i;
  Byte fcf;

  //Load Next Clause Table
  int nctSize=(int)LD_FILE_GET2();
  ///\todo Check on the space requirements of the implgoal table.
  WordPtr tab = LD_LOADER_ExtendModSpace(ent, MEM_IMPL_FIX_SIZE + nctSize);

  int cst;
  int tabSize;

  MEM_implPutLTS(tab,nctSize);
  for(i=0;i<nctSize;i++)
  {
	// If we are loading a query, constant indices should be absolute
	cst=(int)LD_CONST_GetConstIndQuery(query);
	MEM_implPutLT(tab,i,cst);
  }
  
  //Load FindCodeFunc
#define FCF_SEQNSEARCH 1
#define FCF_HASHSEARCH 2
///\todo Correct and move these: shared with addcode.c
  fcf=LD_FILE_GET1();
  if(fcf==FCF_SEQNSEARCH)
  {
    MEM_implPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_SeqnSrch);
    LD_SEARCHTAB_LoadSeqSTab(ent,&tabSize,query);
    MEM_implPutPSTS(tab,tabSize);
    ///\todo do something with returned address.
  }
  else if(fcf==FCF_HASHSEARCH)
  {
    MEM_implPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_HashSrch);
    LD_SEARCHTAB_LoadHashTab(ent,&tabSize,query);
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

void LD_IMPLGOAL_Cleanup()
{
  if(LD_IMPLGOAL_ImplGoals){
	free(LD_IMPLGOAL_ImplGoals);
    LD_IMPLGOAL_ImplGoals=NULL;
  }
}
