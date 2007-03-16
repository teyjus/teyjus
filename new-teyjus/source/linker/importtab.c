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
  struct Vector LConstInds;
  struct Vector extPred; //ConstInd vector 
  PredInfoTab newPred;
  Byte findCodeFun;
  struct Vector findCodeTabs;
}TImportTab_t;

TImportTab_t* CT;  //Import Table of CM.

struct Vector ImportTabs;
ImportTabInd CTID;

void WriteImportTab(TImportTab_t* ImportTab);
void ResolvePredCalls(PredInfo* pred);
PredInfo* FindPredInfo(PredInfoTab* newPred, ConstInd index);

void InitInfoTab(PredInfoTab* newPred);
void AddInfo(PredInfoTab* newPred, ConstInd index);
void AddPredCall(PredInfo* pred, CodeInd addr, int exec_flag);

//Initialize the list of import tables
void InitTImportTabs()
{
  CT=NULL;
  InitVec(&ImportTabs,8,sizeof(TImportTab_t));
  CTID=-1;
}

//Add a new import table to the list.
ImportTabInd NewImportTab()
{
  ImportTabInd tmp=CTID;
  CTID=Extend(&ImportTabs,1);
  CT=Fetch(&ImportTabs,CTID);
  CT->parent=tmp;
  InitVec(&(CT->LConstInds),32,sizeof(ConstInd));
  InitVec(&(CT->findCodeTabs),2,sizeof(HashTab_t));
  InitInfoTab(&(CT->newPred));
  return CTID;
}

//Load the Import tab of the top level module.
void TopImportTab()
{
  
  printf("Loading Top Level Import Tab.\n");//DEBUG
  int i;
  //Use next clause table for top level predicate names.
  TwoBytes count=GET2();
  PredInfoTab* info=&(CT->newPred);
  
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd());
  }
  
  //Add exportdef global predicates to info table.
  count=GET2();
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd());
  }
  
  //Add local predicates to info table.
  count=GET2();
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd());
  }
  
  //Set findCodeFun
  CT->findCodeFun=GET1();
  
  //Load the findCodeTable
  struct Vector* vec=&(CT->findCodeTabs);
  HashTab_t* httmp=(HashTab_t*)Fetch(vec,Extend(vec,1));
  LoadHashTab(httmp);
}

//Load the Import tab of an accumulated module.
void AccImportTab()
{
  
  printf("Loading Accumulated Import Tab.\n");//DEBUG
  int i;
  PredInfoTab* info=&(CT->newPred);
  
  //Ignore next clause table
  TwoBytes count=GET2();
  for(i=0;i<count;i++)
  {
    GetConstInd();
  }
  
  //Ignore exportdef global predicates
  count=GET2();
  for(i=0;i<count;i++)
  {
    GetConstInd();
  }
  
  
  //Add local predicates to info table.
  count=GET2();
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd());
  }
  
  //Ignore findCodeFun
  GET1();
  
  //Load another findCodeTable
  struct Vector* vec=&(CT->findCodeTabs);
  
  HashTab_t* tabaddr=(HashTab_t*)Fetch(vec,Extend(vec,1));
  LoadHashTab(tabaddr);
}

//Load the Import tab of an imported module.
void ImpImportTab()
{
  int i;
  struct Vector* vec;
  ImportTabInd par=CT->parent;
  PredInfoTab* info=&(CT->newPred);
  
  //Set next clause table and mark contents dynamic
  TwoBytes count=GET2();
  
  vec=&(CT->extPred);
  InitVec(vec,(int)count,sizeof(ConstInd));
  Extend(vec,(int)count);
  
  ConstInd* tmp;
  tmp=Fetch(vec,0);
    
  for(i=0;i<count;i++)
  {
    tmp[i]=GetConstInd();
    AddInfo(info,tmp[i]);
    MarkDynamic(CTID,tmp[i]);
    MarkDynamic(par,tmp[i]);
  }
  
  //Mark exportdef global predicates dynamic in the parent,
  //and add them to the predicate info table as new statics.
  ConstInd index;
  for(i=0;i<count;i++)
  {
    index=GetConstInd();
    MarkDynamic(par,index);
    AddInfo(info,index);
  }
    
  //Add local constants to the predicate info table.
  for(i=0;i<count;i++)
  {
    AddInfo(info,GetConstInd());
  }
  
  //Set findCodeFun
  CT->findCodeFun=GET1();
    
  //Load find code table
  vec=&(CT->findCodeTabs);
  LoadHashTab((HashTab_t*)Fetch(vec,Extend(vec,1)));
}

//Resolve the current import table and restore the current table pointer
//and current table ID to thier previous values.
void RestoreImportTab()
{
  printf("Restoring Import Tab\n");//DEBUG
  //Resolve predicate collisions
  int i;
  int size=(CT->findCodeTabs).numEntries;
  HashTab_t* tmp=(HashTab_t*)Fetch(&(CT->findCodeTabs),0);
  for(i=1;i<size;i++)
  {
    MergeFindCodeTabs(tmp,tmp+i);
  }
  //Restore CTID and CT
  CTID=CT->parent;
  if(CTID!=-1)
    CT=Fetch(&ImportTabs,CTID);
}

//Write out all import tables to file.
void WriteImportTabs()
{
  int i;
  PUT2(ImportTabs.numEntries-1);
  TImportTab_t* tmp=(TImportTab_t*)Fetch(&ImportTabs,0);
  for(i=1;i<ImportTabs.numEntries;i++)
  {
    WriteImportTab(tmp+i);
  }
}

//Write out the addcode table for the top level.
void WriteAddCodeTable()
{
  PUT1(1);//FIND_CODE_FUNCTION
  //Write the contents of the primary hash table of the first import table.
  WriteHashTab((HashTab_t*)Fetch(&(((TImportTab_t*)Fetch(&ImportTabs,0))->findCodeTabs),0));
}

//Write out a single import table to file.
void WriteImportTab(TImportTab_t* ImportTab)
{
  int i;
  struct Vector* vec=&(ImportTab->LConstInds);
  int count=vec->numEntries;
  ConstInd* tmp=Fetch(vec,0);
  for(i=0;i<count;i++)
  {
    PutConstInd(tmp[i]);
  }
  
  vec=&(ImportTab->extPred);
  count=vec->numEntries;
  tmp=Fetch(vec,0);
  for(i=0;i<count;i++)
  {
    PutConstInd(tmp[i]);
  }
  
  PUT1(1);//FIND_CODE_FUNCTION
  
  WriteHashTab((HashTab_t*)Fetch(&(ImportTab->findCodeTabs),0));
}

///////////////////////////////////////////////////////////////////////////////
//Definitions of functions used to manage the Predicate Info Tables////////////
///////////////////////////////////////////////////////////////////////////////

void InitInfoTab(PredInfoTab* newPred)
{
  InitVec(newPred,16,sizeof(PredInfo));
}

//Add the predicate with index 'index' to the predicate info table,
//and mark the new entry as a static with no calls.
void AddInfo(PredInfoTab* newPred, ConstInd index)
{
  PredInfo* tmp=(PredInfo*)Fetch(newPred,Extend(newPred,1));
  tmp->index=index;
  tmp->dynamic_flag=0;
  InitVec(&(tmp->predCalls),8,sizeof(PCallEnt));
}

void AddPredCall(PredInfo* pred, CodeInd addr, int exec_flag)
{
  PCallEnt* tmp = (PCallEnt*)Fetch(&(pred->predCalls),Extend(&(pred->predCalls),1));
  tmp->addr=addr;
  tmp->exec_flag=exec_flag;
}

//Return a pointer to the Predicate info table entry for the constant with index 'index'.
PredInfo* FindPredInfo(PredInfoTab* newPred, ConstInd index)
{
  PredInfo* tmp=Fetch(newPred,0);
  int i;
  int size=newPred->numEntries;
  
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
  PredInfo* tmp=FindPredInfo(&(((TImportTab_t*)Fetch(&ImportTabs,tab))->newPred),index);
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
  int size=(pred->predCalls).numEntries;
  PCallEnt* tmp=Fetch(&(pred->predCalls),0);
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
    
    addr=HashCodeAddr((HashTab_t*)Fetch(&(CT->findCodeTabs),0),pred->index);
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
