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
#include <stdio.h>
#include <stdlib.h>
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "module.h"
#include "hashtab.h"
#include "code.h"
#include "CallResolution.h"
#include "VectorRW.h"
#include "linker_message.h"

//////////////////////////////////////////////////////
//ImportTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  ImportTabInd parent;
  Byte numSegs;
  struct Vector NextClauseTable;
  struct Vector ExportDefPreds;//This should probably be some kind of hash.
  struct Vector LConstInds;
  ConstInd LowLConst;
  PredInfoTab* newPred;
  Byte findCodeFun;
  struct Vector findCodeTabs;
}TImportTab_t;

TImportTab_t* CT;  //Import Table of CM.

struct Vector ImportTabs;
ImportTabInd CTID;

PredInfoTab* GetPredInfoTab()
{
  return CT->newPred;
}

//Initialize the list of import tables
void InitTImportTabs()
{
  CT=NULL;
  LK_VECTOR_Init(&ImportTabs,8,sizeof(TImportTab_t));
  CTID=-1;
}

//Add a new import table to the list.
ImportTabInd NewImportTab()
{
  ImportTabInd tmp=CTID;
  CTID=LK_VECTOR_Grow(&ImportTabs,1);
  CT=(TImportTab_t *)LK_VECTOR_GetPtr(&ImportTabs,CTID);
  CT->parent=tmp;
  CT->numSegs=0;
  LK_VECTOR_Init(&(CT->NextClauseTable),32,sizeof(ConstInd));
  LK_VECTOR_Init(&(CT->ExportDefPreds),32,sizeof(ConstInd));
  LK_VECTOR_Init(&(CT->LConstInds),32,sizeof(ConstInd));
  LK_VECTOR_Init(&(CT->findCodeTabs),2,sizeof(HashTab_t));
  CT->newPred=(PredInfoTab *)EM_malloc(sizeof(PredInfoTab));
  InitInfoTab(CT->newPred);
  return CTID;
}

void AddLocalConstants(struct Vector* LConstInds, struct Module_st* CMData)
{
  ConstInd* tmp;
  int i;
  ConstInd con;
    
  mutter("Filling in local constants\n");
  if(0<CMData->LConstAdj.count)
  {
    con.gl_flag=LOCAL;
    con.index=CMData->LConstAdj.offset;
    tmp = (ConstInd *)LK_VECTOR_GetPtr(LConstInds,LK_VECTOR_Grow(LConstInds,CMData->LConstAdj.count));
    for(i=0;i<CMData->LConstAdj.count;i++)
    {
      tmp[i]=con;
      (con.index)++;
    }
  }
}

void WriteLocalConstant(int fd, void* entry)
{
  PutConstInd(fd,*(ConstInd*)entry);
}

Boolean isImportLocal(ConstInd ind)
{
  if(ind.gl_flag == LOCAL)
  {
    if(CT->LowLConst.index<=ind.index){
      return TRUE;
    }
  }
  return FALSE;
}

Boolean isImportExportDef(ConstInd ind)
{
  struct Vector* vec;
  int size;
  ConstInd* table;
  int i;
  //Test for exportdef global
  vec = &(CT->ExportDefPreds);
  size = LK_VECTOR_Size(vec);
  table=(ConstInd*)LK_VECTOR_GetPtr(vec,0);
  for(i=0;i<size;i++)
  {
    if(table[i].index==ind.index && 
       table[i].gl_flag==ind.gl_flag)
    {
      return TRUE;
    }
  }
  return FALSE;
}

Boolean isInNextClauseTable(ConstInd ind)
{
  int i;
  struct Vector* vec;
  int size;
  ConstInd* table;
  //Doing sequential search may be a performance issue.
  vec = &(CT->NextClauseTable);
  size = LK_VECTOR_Size(vec);
  table=(ConstInd*)LK_VECTOR_GetPtr(vec,0);
  for(i=0;i<size;i++)
  {
    if(table[i].index==ind.index && 
       table[i].gl_flag==ind.gl_flag)
    {
      return TRUE;
    }
  }
  return FALSE;
}

Boolean shouldTidySwitchOnReg(ConstInd ind)
{
  return isImportLocal(ind) || isImportExportDef(ind);
}

///Add the index if it isn't already there
///and isn't local or exportdef in the top level
void ConditionallyAddToNextClauseTable(ConstInd ind)
{
  struct Vector* vec;
  ConstInd* table;

  if(isImportLocal(ind))
    return;
  
  if(isImportExportDef(ind))
    return;
  
  if(isInNextClauseTable(ind))
    return;
  
  //Add constant to the next clause table.
  vec = &(CT->NextClauseTable);
  table=(ConstInd*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1));
  *table=ind;
}

//Load the Import tab of the top level module.
void TopImportTab(int fd, struct Module_st* CMData)
{
  int i;
  struct Vector* nct_vec;
  TwoBytes count;
  ConstInd* nct;
  struct Vector* expd_vec;
  ConstInd* expd;
  struct Vector* vec;
  HashTab_t* httmp;

  mutter("Loading Top Level Import Tab.\n");

  AddLocalConstants(&(CT->LConstInds),CMData);
  CT->LowLConst.index=0;
  CT->LowLConst.gl_flag=LOCAL;
  
  //Use next clause table for top level predicate names.
  count=LK_FILE_GET2(fd);
  nct_vec = &CT->NextClauseTable;
  nct=(ConstInd*)LK_VECTOR_GetPtr(nct_vec,LK_VECTOR_Grow(nct_vec,count));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
  }
  
  //Ignore exportdef predicates.
  count=LK_FILE_GET2(fd);
  expd_vec = &CT->ExportDefPreds;
  expd=(ConstInd*)LK_VECTOR_GetPtr(expd_vec,LK_VECTOR_Grow(expd_vec,count));
  for(i=0;i<count;i++)
  {
    expd[i]=GetConstInd(fd,CMData);
  }
  
  //Ignore local predicates.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    GetConstInd(fd,CMData);
  }
  
  //Set findCodeFun
  CT->findCodeFun=LK_FILE_GET1(fd);
  
  //Load the findCodeTable
  vec=&(CT->findCodeTabs);
  httmp=(HashTab_t*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1));
  LoadHashTab(fd,CMData,httmp);
}

//Load the Import tab of an accumulated module.
void AccImportTab(int fd, struct Module_st* CMData)
{
  int i;
  TwoBytes count;
  struct Vector* vec;
  HashTab_t* tabaddr;

  mutter("Loading Accumulated Import Tab.\n");  
  
  //Ignore next clause table
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    ConditionallyAddToNextClauseTable(GetConstInd(fd,CMData));
  }
  
  //These are added later to reduce search times
  AddLocalConstants(&(CT->LConstInds),CMData);
  
  //Ignore exportdef global predicates
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    GetConstInd(fd,CMData);
  }
  
  //Ignore local predicates.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    GetConstInd(fd,CMData);
  }
  
  //Ignore findCodeFun
  LK_FILE_GET1(fd);
  
  //Load another findCodeTable
  vec=&(CT->findCodeTabs);
  
  tabaddr=(HashTab_t*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1));
  LoadHashTab(fd,CMData,tabaddr);
}

//Load the Import tab of an imported module.
void ImpImportTab(int fd, struct Module_st* CMData)
{
  int i;
  struct Vector* vec;
  PredInfoTab* CMInfoTab;
  PredInfoTab* ParInfoTab;
  TwoBytes count;
  struct Vector* nct_vec;
  ConstInd* nct;
  struct Vector* expd_vec;
  ConstInd* expd;

  AddLocalConstants(&(CT->LConstInds),CMData);
  CT->LowLConst.index=CMData->LConstAdj.offset;
  CT->LowLConst.gl_flag=LOCAL;
  CMInfoTab=CT->newPred;
  ParInfoTab=((TImportTab_t*)LK_VECTOR_GetPtr(&ImportTabs,CT->parent))->newPred;
  //Set next clause table and mark contents dynamic
  count=LK_FILE_GET2(fd);
  nct_vec = &CT->NextClauseTable;
  nct=(ConstInd*)LK_VECTOR_GetPtr(nct_vec,LK_VECTOR_Grow(nct_vec,count));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
    MarkDynamic(CMInfoTab,nct[i]);
    MarkDynamic(ParInfoTab,nct[i]);
  }
  
  //Mark exportdef global predicates dynamic in the parent,
  //and add them to the predicate info table as new statics.
  count=LK_FILE_GET2(fd);
  expd_vec = &CT->ExportDefPreds;
  expd=(ConstInd*)LK_VECTOR_GetPtr(expd_vec,LK_VECTOR_Grow(expd_vec,count));
  for(i=0;i<count;i++)
  {
    expd[i]=GetConstInd(fd,CMData);
    MarkDynamic(ParInfoTab,expd[i]);
  }
    
  //Ignore local predicates.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    GetConstInd(fd,CMData);
  }
  
  //Set findCodeFun
  CT->findCodeFun=LK_FILE_GET1(fd);
    
  //Load find code table
  vec=&(CT->findCodeTabs);
  LoadHashTab(fd,CMData,(HashTab_t*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1)));
}

void LK_IMPORT_AssignSegmentId(struct Module_st* CMData)
{
  CMData->SegmentID=++(CT->numSegs);
}

//Resolve the current import table and restore the current table pointer
//and current table ID to thier previous values.
void RestoreImportTab()
{
  int i;
  int size=LK_VECTOR_Size(&(CT->findCodeTabs));
  HashTab_t* tmp;

  mutter("Restoring Import Tab\n");

  //Resolve predicate collisions
  tmp=(HashTab_t*)LK_VECTOR_GetPtr(&(CT->findCodeTabs),0);
  mutter("After get\n");
  for(i=1;i<size;i++)
  {
    MergeFindCodeTabs(tmp,tmp+i);
  }
  
  mutter("Resolving predicate calls\n");
  ResolvePredCalls(CT->newPred,tmp);
  
  //Restore CTID and CT
  CTID=CT->parent;
  if(CTID!=-1)
    CT=(TImportTab_t *)LK_VECTOR_GetPtr(&ImportTabs,CTID);
}

void WriteNctEntry(int fd, void* entry)
{
  PutConstInd(fd,*(ConstInd*)entry);
}

//Write out a single import table to file.
void WriteImportTab(int fd, void* entry)
{
  TImportTab_t* ImportTab=(TImportTab_t*)entry;
  debug("Writing an import table.\n");
  LK_FILE_PUT1(fd,ImportTab->numSegs);
  
  LK_VECTOR_Write(fd,&(ImportTab->NextClauseTable),WriteNctEntry);
  
  LK_VECTOR_Write(fd,&(ImportTab->LConstInds),WriteLocalConstant);
  
  LK_FILE_PUT1(fd,1);//FIND_CODE_FUNCTION
  
  WriteHashTab(fd,(HashTab_t*)LK_VECTOR_GetPtr(&(ImportTab->findCodeTabs),0));
}

//Write out all import tables to file.
void WriteImportTabs(int fd)
{
  detail("Writing out import tables.\n");
  LK_VECTOR_Write(fd,&ImportTabs,WriteImportTab);
}
