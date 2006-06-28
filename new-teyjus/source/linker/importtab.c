#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "module.h"
#include "hashtab.h"
//////////////////////////////////////////////////////
//ImportTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	ConstInd index;
	CodeInd addr;
}PCallEnt;

typedef struct{
	struct Vector LConstInds;
	struct Vector extPred;
	INT1 findCodeFun;
	struct Vector findCodeTabs;
}TImportTab_t;

TImportTab_t* CT;	//Import Table of CM.

struct Vector DynamicPreds;
struct Vector predCalls;
struct Vector ImportTabs;
ImportTabInd CTID;

void WriteImportTab(TImportTab_t* ImportTab);

void MarkDynamic(ConstInd index)
{
	//TODO
}

void ResolvePredCalls()
{
	//TODO
}

void RestoreImportTab()
{
	//Resolve predicate collisions
	int i;
	int size=(CT->findCodeTabs).numEntries;
	struct Vector* tmp=(struct Vector*)Fetch(&(CT->findCodeTabs),i);
	for(i=1;i<size;i++)
		MergeFindCodeTabs(tmp,tmp+i);
		
	//Restore CTID and CT
	CTID=CM->ImportTabID;
	CT=Fetch(&ImportTabs,CTID);
}

void InitTImportTabs()
{
	InitVec(&ImportTabs,8,sizeof(TImportTab_t));
	InitVec(&predCalls,128,sizeof(PCallEnt));
}

void NewImportTab()
{
	CM->ImportTabID=CTID;
	CTID=Extend(&ImportTabs,1);
	CT=Fetch(&ImportTabs,CTID);
	InitVec(&(CT->LConstInds),32,sizeof(ConstInd));
	InitVec(&(CT->findCodeTabs),1,sizeof(struct Vector));
}

void ExtImportTab()
{
	int i;
	//Ignore next clause table
	INT2 count=GET2();
	for(i=0;i<count;i++)
	{
		GetConstInd();
	}
	
	//Ignore findCodeFun
	GET1();
	
	//Load another findCodeTable
	struct Vector* vec=&(CT->findCodeTabs);
	LoadHashTab((struct Vector*)Fetch(vec,Extend(vec,1)));
}

void GetImportTab()
{
	int i;
	ConstInd* tmp;
	struct Vector* vec;
	
	//Set next clause table
	INT2 count=GET2();
	
	vec=&(CT->extPred);
	InitVec(vec,(int)count,sizeof(ConstInd));
	Extend(vec,(int)count);
	tmp=Fetch(vec,0);
		
	for(i=0;i<count;i++)
	{
		tmp[i]=GetConstInd();
	}
		
	//Set findCodeFun
	CT->findCodeFun=GET1();
		
	//Load find code table
	vec=&(CT->findCodeTabs);
	LoadHashTab((struct Vector*)Fetch(vec,Extend(vec,1)));
}

void WriteImportTabs()
{
	int i;
	PUT2(ImportTabs.numEntries);
	TImportTab_t* tmp=(TImportTab_t*)Fetch(&ImportTabs,0);
	for(i=0;i<ImportTabs.numEntries;i++)
	{
		WriteImportTab(tmp+i);
	}
}

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
	
	WriteHashTab((struct Vector*)Fetch(&(ImportTab->findCodeTabs),0));
}

void PushCall(ConstInd index,CodeInd addr)
{
	PCallEnt* pCall=(PCallEnt*)Fetch(&predCalls,Extend(&predCalls,1));
	pCall->index=index;
	pCall->addr=addr;
}
