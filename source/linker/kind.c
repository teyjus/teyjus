#include <stdio.h>
#include <stdlib.h>
#include "datatypes.h"
#include "module.h"
#include "kind.h"
#include "vector.h"
#include "rename.h"

/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	int arity;
	Name name;
}TGKind_t;

struct Vector GKinds;

KindInd* AllocateLGKinds(int count);
KindInd LoadTopGKind(int i);
KindInd LoadGKind();
void WriteGKind(int i);

void InitTGKinds()
{
	InitVec(&GKinds,128,sizeof(TGKind_t));
}

KindInd* AllocateLGKinds(int count)
{
	KindInd* tmp;
	if(count>0)
	{
		tmp=(KindInd*)malloc(count*sizeof(KindInd));
		if(tmp==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	else
	{
		tmp=NULL;
	}
	
	return tmp;
}

void LoadGKinds()
{
	int i;
	
	INT2 count=CM->GKindcount=GET2();
	CM->GKind=AllocateLGKinds(count);
	for(i=0;i<count;i++)
	{
		CM->GKind[i]=LoadGKind();
	}
}

KindInd LoadGKind()
{
	int arity=GET1();
	int oldarity;
	Name name;
	GetName(&name);
	KindInd index=RenameKind(name);
	oldarity=CheckKindArity(index);
	if(arity!=oldarity)
	{
		printf("Kind Arity Mismatch: Kind %s should have arity %d, has arity %d.\n",name.string,oldarity,arity);
		exit(0);
	}
	Clear(name);
	return index;
}

void LoadTopGKinds()
{
	int i;
	INT2 count=CM->GKindcount=GET2();
	Extend(&GKinds,(int)count);
	CM->GKind=AllocateLGKinds(count);
	for(i=0;i<count;i++)
	{
		CM->GKind[i]=LoadTopGKind(i);
	}
}

KindInd LoadTopGKind(int i)
{
	KindInd tmp;
	TGKind_t* tmp2=Fetch(&GKinds,i);
	tmp.index=i;
	tmp.gl_flag=GLOBAL;
	
	tmp2->arity=GET1();
	GetName(&(tmp2->name));
	
	return tmp;
}

void WriteGKinds()
{
	int i;
	INT2 tmp=GKinds.numEntries;
	PUT2(tmp);
	for(i=0;i<tmp;i++)
	{
		WriteGKind(i);
	}
}

void WriteGKind(i)
{
	TGKind_t* tmp=Fetch(&GKinds,i);
	PUT1(tmp->arity);
	PutName(tmp->name);
}


//////////////////////////////////////////////////////
//LKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	int arity;
}TLKind_t;

struct Vector LKinds;

void LoadLKind(int i);
void WriteLKind(int i);

void InitTLKinds()
{
	InitVec(&LKinds,128,sizeof(TLKind_t));
}

void LoadLKinds()
{
	int i;
	INT2 count=CM->LKindcount=GET2();
	int offset=CM->LKindoffset=Extend(&LKinds,count);
	//CM->LKind=AllocateLLKinds(count);
	for(i=0;i<count;i++)
	{
		LoadLKind(offset+i);
	}
}

void LoadLKind(int i)
{
	TLKind_t* tmp=Fetch(&LKinds,i);
	tmp->arity=GET1();
}

void WriteLKinds()
{
	int i;
	INT2 tmp=LKinds.numEntries;
	PUT2(tmp);
	for(i=0;i<tmp;i++)
	{
		WriteLKind(i);
	}
}

void WriteLKind(i)
{
	TLKind_t* tmp=Fetch(&LKinds,i);
	PUT1(tmp->arity);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckKindArity(KindInd i)
{
	if(i.gl_flag==LOCAL)
	{
		return ((TLKind_t*)Fetch(&LKinds,i.index))->arity;
	}
	//GLOBAL_KIND
	return ((TGKind_t*)Fetch(&GKinds,i.index))->arity;
}
