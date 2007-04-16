#include <stdio.h>
#include <stdlib.h>
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "module.h"
#include "hashtab.h"
#include "code.h"
//////////////////////////////////////////////////////
//ImportTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  ConstInd index;
  int dynamic_flag;
  struct Vector predCalls;
}PredInfo;

typedef struct Vector PredInfoTab;

typedef struct{
  int exec_flag;
  CodeInd addr;
}PCallEnt;

typedef struct{
  ImportTabInd parent;
  Byte numSegs;
  int NctSize;
  ConstInd* NextClauseTable;
  struct Vector LConstInds;
  PredInfoTab newPred;
  Byte findCodeFun;
  struct Vector findCodeTabs;
}TImportTab_t;

TImportTab_t* CT;  //Import Table of CM.

struct Vector ImportTabs;
ImportTabInd CTID;

void ResolvePredCalls(PredInfo* pred);
PredInfo* FindPredInfo(PredInfoTab* newPred, ConstInd index);

void InitInfoTab(PredInfoTab* newPred);
void AddInfo(PredInfoTab* newPred, ConstInd index);
void AddPredCall(PredInfo* pred, CodeInd addr, int exec_flag);

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
  InitInfoTab(&(CT->newPred));
  return CTID;
}

void AddLocalConstants(struct Vector* LConstInds, struct Module_st* CMData)
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

void WriteLocalConstant(int fd, void* entry)
{
  PutConstInd(fd,*(ConstInd*)entry);
}

//Load the Import tab of the top level module.
void TopImportTab(int fd, struct Module_st* CMData)
{
  
  printf("Loading Top Level Import Tab.\n");//DEBUG
  int i;
  
  CMData->SegmentID=CT->numSegs=1;
  AddLocalConstants(&(CT->LConstInds),CMData);
  
  PredInfoTab* info=&(CT->newPred);
  //Use next clause table for top level predicate names.
  TwoBytes count=LK_FILE_GET2(fd);
  ConstInd* nct=CT->NextClauseTable=EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
    AddInfo(info,nct[i]);
  }
  
  //Add exportdef global predicates to info table.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd(fd,CMData));
  }
  
  //Add local predicates to info table.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd(fd,CMData));
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
  
  printf("Loading Accumulated Import Tab.\n");//DEBUG
  int i;
  
  CMData->SegmentID=(CT->numSegs)++;
  AddLocalConstants(&(CT->LConstInds),CMData);
  
  PredInfoTab* info=&(CT->newPred);
  
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
  
  
  //Add local predicates to info table.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd(fd,CMData));
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
  ImportTabInd par=CT->parent;
  
  CMData->SegmentID=CT->numSegs=1;
  AddLocalConstants(&(CT->LConstInds),CMData);
  PredInfoTab* info=&(CT->newPred);
  
  //Set next clause table and mark contents dynamic
  TwoBytes count=LK_FILE_GET2(fd);
  ConstInd* nct=CT->NextClauseTable=EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    nct[i]=GetConstInd(fd,CMData);
    AddInfo(info,nct[i]);
    MarkDynamic(CTID,nct[i]);
    MarkDynamic(par,nct[i]);
  }
  
  //Mark exportdef global predicates dynamic in the parent,
  //and add them to the predicate info table as new statics.
  ConstInd index;
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    index=GetConstInd(fd,CMData);
    MarkDynamic(par,index);
    AddInfo(info,index);
  }
    
  //Add local predicates to the predicate info table.
  count=LK_FILE_GET2(fd);
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd(fd,CMData));
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
  printf("Restoring Import Tab\n");//DEBUG
  //Resolve predicate collisions
  int i;
  int size=LK_VECTOR_Size(&(CT->findCodeTabs));
  HashTab_t* tmp=(HashTab_t*)LK_VECTOR_GetPtr(&(CT->findCodeTabs),0);
  for(i=1;i<size;i++)
  {
    MergeFindCodeTabs(tmp,tmp+i);
  }
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
  LK_VECTOR_Write(fd,&ImportTabs,WriteImportTab);
}

///////////////////////////////////////////////////////////////////////////////
//Definitions of functions used to manage the Predicate Info Tables////////////
///////////////////////////////////////////////////////////////////////////////

void InitInfoTab(PredInfoTab* newPred)
{
  LK_VECTOR_Init(newPred,16,sizeof(PredInfo));
}

//Add the predicate with index 'index' to the predicate info table,
//and mark the new entry as a static with no calls.
void AddInfo(PredInfoTab* newPred, ConstInd index)
{
  PredInfo* tmp=(PredInfo*)LK_VECTOR_GetPtr(newPred,LK_VECTOR_Grow(newPred,1));
  tmp->index=index;
  tmp->dynamic_flag=0;
  LK_VECTOR_Init(&(tmp->predCalls),8,sizeof(PCallEnt));
}

void AddPredCall(PredInfo* pred, CodeInd addr, int exec_flag)
{
  PCallEnt* tmp = (PCallEnt*)LK_VECTOR_GetPtr(&(pred->predCalls),LK_VECTOR_Grow(&(pred->predCalls),1));
  tmp->addr=addr;
  tmp->exec_flag=exec_flag;
}

//Return a pointer to the Predicate info table entry for the constant with index 'index'.
PredInfo* FindPredInfo(PredInfoTab* newPred, ConstInd index)
{
  PredInfo* tmp=LK_VECTOR_GetPtr(newPred,0);
  int i;
  int size=LK_VECTOR_Size(newPred);
  
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==index.gl_flag)&&(tmp[i].index.index==index.index))
      return tmp+i;
  }
  
  return NULL;
}

//Mark the constant with index 'index' as a dynamic predicate in the import tables
//that has index 'tab'.
void MarkDynamic(ImportTabInd tab, ConstInd index)
{
  PredInfo* tmp=FindPredInfo(&(((TImportTab_t*)LK_VECTOR_GetPtr(&ImportTabs,tab))->newPred),index);
  if(tmp==NULL)
  {
    printf("Error couldn't find predicate to mark dynamic.");
    exit(0);
  }
  tmp->dynamic_flag++;
}

//Resolve all of the predicate call listed in the predicate table entry at 'pred'.
void ResolvePredCalls(PredInfo* pred)
{
  int i;
  int size=LK_VECTOR_Size(&(pred->predCalls));
  PCallEnt* tmp=LK_VECTOR_GetPtr(&(pred->predCalls),0);
  CodeInd addr;
  if(pred->dynamic_flag>0)
  {
    for(i=0;i<size;i++)
    {
      MakeCallName(tmp[i].addr,tmp[i].exec_flag,pred->index);
    }
  }
  else
  {
    
    addr=HashCodeAddr((HashTab_t*)LK_VECTOR_GetPtr(&(CT->findCodeTabs),0),pred->index);
    for(i=0;i<size;i++)
    {
      MakeCall(tmp[i].addr,tmp[i].exec_flag,addr);
    }
  }
}

//Add a call to the predicate with index 'index' at code offset 'addr' and specify
//call style with exec_flag to the current import table's predicate info table.
void PushCall(ConstInd index,CodeInd addr,int exec_flag)
{
  AddPredCall(FindPredInfo(&(CT->newPred),index),addr,exec_flag);
}
