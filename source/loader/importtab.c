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
#include "ld_message.h"
#include "searchtab.h"

TwoBytes LD_IMPORTTAB_numImportTabs;
WordPtr* LD_IMPORTTAB_ImportTabs;

WordPtr LD_IMPORTTAB_LoadImportTab(MEM_GmtEnt* ent);

void LD_IMPORTTAB_LoadImportTabs(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_IMPORTTAB_numImportTabs=LD_FILE_GET2();
  LD_mutter("Loading %d import tables\n",count);
  LD_IMPORTTAB_ImportTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  EM_TRY{
    ent->addtable=(CSpacePtr)LD_IMPORTTAB_LoadImportTab(ent);
  }EM_CATCH{
    LD_error("While loading add code table\n");
    EM_RETHROW();
  }
  for(i=1;i<count;i++)
  {
    LD_debug("i=%d\n",i);
    EM_TRY{
      LD_IMPORTTAB_ImportTabs[i]=LD_IMPORTTAB_LoadImportTab(ent);
    }EM_CATCH{
      LD_error("While loading import table %d\n",i);
      EM_RETHROW();
    }
  }
  
  return;
}

// Never called from a query
WordPtr LD_IMPORTTAB_LoadImportTab(MEM_GmtEnt* ent)
{
  WordPtr tab;
  int psts;
  int cst;
  int i;
  int numSegs;
  int nctSize;
  int lctSize;
  Byte fcf;
  MemPtr lcTab;

  LD_debug("Loading import table\n");
  ///\todo Check on the space requirements of the import table.

  tab=LD_LOADER_ExtendModSpace(ent,MEM_IMP_FIX_SIZE);
  numSegs = LD_FILE_GET1();//Get number of segments.
  MEM_impPutNCSEG(tab,numSegs);
  LD_debug(" Import table has %d segments\n",numSegs);
  //Load Next Clause Table
  nctSize=(int)LD_FILE_GET2();
  LD_debug(" Next clause table has %d entries\n",nctSize);
  
  MEM_impPutLTS(tab,nctSize);

  // added by XQ
  LD_LOADER_ExtendModSpace(ent, nctSize);
  for(i=0;i<nctSize;i++)
  {      
    cst=(int)LD_CONST_GetConstInd();
    MEM_impPutLT(tab,i,cst);
  }
  
  //Local Constant table
  lctSize=(int)LD_FILE_GET2();
  LD_debug(" Local constant table has %d entries\n",lctSize);
  
  MEM_impPutNLC(tab, lctSize);
  lcTab=MEM_impLCT(tab,nctSize);
  ///\todo Reorder Link table and local constant table
  // added by XQ
  LD_LOADER_ExtendModSpace(ent, lctSize);
  
  //for(i=0;i<nctSize;i++) -- XQ
  for (i = 0; i < lctSize; i++)
  {
    cst=(int)LD_CONST_GetConstInd();
    MEM_impPutLCT(lcTab, i, cst);
  }
  
  //Load FindCodeFunc
  fcf=LD_FILE_GET1();
  if(fcf==SEARCHTAB_FCF_SEQNSEARCH)
  {
    LD_debug(" Loading sequential search table\n");
    MEM_impPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_SeqnSrch);
    LD_SEARCHTAB_LoadSeqSTab(ent,&psts,0);
    MEM_impPutPSTS(tab,psts);
    ///\todo do something with returned address.
  }
  else if(fcf==SEARCHTAB_FCF_HASHSEARCH)
  {
      
    LD_debug(" Loading hash table\n");
    MEM_impPutFC(tab,(MEM_FindCodeFnPtr)&LD_SEARCHTAB_HashSrch);
    LD_SEARCHTAB_LoadHashTab(ent,&psts,0);
    MEM_impPutPSTS(tab,psts);
    ///\todo do something with returned address.
  } else {
    LD_error("Invalid find code function %d\n",fcf);
    EM_THROW(LD_LoadError);
  }
  LD_debug("Done Loading Import table\n");
  return tab;
}

WordPtr LD_IMPORTTAB_GetImportTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_IMPORTTAB_numImportTabs)
    EM_THROW(LD_LoadError);
  return LD_IMPORTTAB_ImportTabs[i];
}

void LD_IMPORTTAB_Cleanup()
{
  if(LD_IMPORTTAB_ImportTabs){
	free(LD_IMPORTTAB_ImportTabs);
    LD_IMPORTTAB_ImportTabs=NULL;
  }
}
