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
#include "message.h"

//////////////////////////////////////////////////////
//ImportTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  ImportTabInd parent;
  Byte numSegs;
  int NctSize;
  ConstInd* NextClauseTable;
  struct Vector LConstInds;
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
  CT=LK_VECTOR_GetPtr(&ImportTabs,CTID);
  CT->parent=tmp;
  LK_VECTOR_Init(&(CT->LConstInds),32,sizeof(ConstInd));
  LK_VECTOR_Init(&(CT->findCodeTabs),2,sizeof(HashTab_t));
  CT->newPred=EM_malloc(sizeof(PredInfoTab));
  InitInfoTab(CT->newPred);
  return CTID;
}

void AddLocalConstants(struct Vector* LConstInds, struct Module_st* CMData)
{
  mutter("Filling in local constants\n");
  if(0<CMData->LConstAdj.count)
  {
    int i;
    ConstInd con;
    con.gl_flag=LOCAL;
    con.index=CMData->LConstAdj.offset;
    ConstInd* tmp = LK_VECTOR_GetPtr(LConstInds,LK_VECTOR_Grow(LConstInds,CMData->LConstAdj.count));
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

//Load the Import tab of the top level module.
void TopImportTab(int fd, struct Module_st* CMData)
{
  mutter("Loading Top Level Import Tab.\n");
  int i;
  
  CMData->SegmentID=CT->numSegs=1;
  AddLocalConstants(&(CT->LConstInds),CMData);
  
  //Use next clause table for top level predicate names.
  TwoBytes count=LK_FILE_GET2(fd);
  ConstInd* nct=CT->NextClauseTable=EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
  }
  
  //Ignore exportdef predicates.
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
  
  //Set findCodeFun
  CT->findCodeFun=LK_FILE_GET1(fd);
  
  //Load the findCodeTable
  struct Vector* vec=&(CT->findCodeTabs);
  HashTab_t* httmp=(HashTab_t*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1));
  LoadHashTab(fd,CMData,httmp);
}

//Load the Import tab of an accumulated module.
void AccImportTab(int fd, struct Module_st* CMData)
{
  mutter("Loading Accumulated Import Tab.\n");
  int i;
  CMData->SegmentID=(CT->numSegs)++;
  AddLocalConstants(&(CT->LConstInds),CMData);
  
  //Ignore next clause table
  TwoBytes count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    GetConstInd(fd,CMData);
  }
  
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
  struct Vector* vec=&(CT->findCodeTabs);
  
  HashTab_t* tabaddr=(HashTab_t*)LK_VECTOR_GetPtr(vec,LK_VECTOR_Grow(vec,1));
  LoadHashTab(fd,CMData,tabaddr);
}

//Load the Import tab of an imported module.
void ImpImportTab(int fd, struct Module_st* CMData)
{
  int i;
  struct Vector* vec;
  CMData->SegmentID=CT->numSegs=1;
  AddLocalConstants(&(CT->LConstInds),CMData);
  PredInfoTab* CMInfoTab=CT->newPred;
  PredInfoTab* ParInfoTab=((TImportTab_t*)LK_VECTOR_GetPtr(&ImportTabs,CT->parent))->newPred;
  //Set next clause table and mark contents dynamic
  TwoBytes count=LK_FILE_GET2(fd);
  ConstInd* nct=CT->NextClauseTable=EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
    MarkDynamic(CMInfoTab,nct[i]);
    MarkDynamic(ParInfoTab,nct[i]);
  }
  
  //Mark exportdef global predicates dynamic in the parent,
  //and add them to the predicate info table as new statics.
  ConstInd index;
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    index=GetConstInd(fd,CMData);
    MarkDynamic(ParInfoTab,index);
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

//Resolve the current import table and restore the current table pointer
//and current table ID to thier previous values.
void RestoreImportTab()
{
  mutter("Restoring Import Tab\n");
  //Resolve predicate collisions
  int i;
  int size=LK_VECTOR_Size(&(CT->findCodeTabs));
  HashTab_t* tmp=(HashTab_t*)LK_VECTOR_GetPtr(&(CT->findCodeTabs),0);
  mutter("After get\n");
  for(i=1;i<size;i++)
  {
    MergeFindCodeTabs(tmp,tmp+i);
  }
  
  mutter("Resolving predicate calls\n",i);
  ResolvePredCalls(CT->newPred,tmp);
  
  //Restore CTID and CT
  CTID=CT->parent;
  if(CTID!=-1)
    CT=LK_VECTOR_GetPtr(&ImportTabs,CTID);
}

//Write out a single import table to file.
void WriteImportTab(int fd, void* entry)
{
  int i;
  TImportTab_t* ImportTab=(TImportTab_t*)entry;
  debug("Writing an import table.\n");
  LK_FILE_PUT1(fd,ImportTab->numSegs);
  
  LK_FILE_PUT2(fd,ImportTab->NctSize);
  for(i=0;i<ImportTab->NctSize;i++)
  {
    PutConstInd(fd,ImportTab->NextClauseTable[i]);
  }
  
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
