#include "toplevel.h"
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

typedef struct{
	int entries;
	int size;
	TX_t* entry;
}GKind_Vec;

GKind_Vec GKinds;

void InitTGKinds()
{
	GKinds.entries=0;
	GKinds.size=0;
	GKinds.entry=NULL;
}

int AllocateTGKinds(int count)
{
	GKinds.entries=count;
	GKinds.size=count;
	malloc(count*sizeof(TGKind_t));
	if(GKinds.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return 0;
}

LGKind_t* AllocateLGKinds(int count)
{
	LGKind_t* tmp=(LGKind_t*)malloc(count*sizeof(LGKind_t));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return tmp;
}

void LoadGKinds()
{
	int count=CM.GKindcount=GET1();
	//int offset=CM.GKindoffset=AllocateTGKinds(count);
	CM.GKind=AllocateLGKinds(count);
	for(int i=0;i<count;i++)
	{
		CM.GKind[i]=LoadGKind();
	}
}

LGKind_t LoadGKind()
{
	int arity=GET1();
	KindInd index=RenameKind(GetName());
	if(arity!=CheckKindArity(index))
	{
		perror("Kind Arity Mismatch");
		exit(0);
	}
	return (LGKind_t)index;
}

void LoadTopGKinds()
{
	int count=CM.GKindcount=GET1();
	AllocateTGKinds(count);
	CM.GKind=AllocateLGKinds(count);
	for(int i=0;i<count;i++)
	{
		CM.GKind[i]=LoadTopGKind(i);
	}
}

LGKind_t LoadTopGKind(int i)
{
	LGKind_t tmp;
	tmp.index.index=i;
	tmp.index.gl_flag=GLOBAL_KIND;

	GKind_Vec.entry[i].arity=GET1();
	GKind_Vec.entry[i].name=GetName();
	
	return tmp;
}

void WriteGKinds()
{
	PUT1(GKinds.entries);
	for(int i=0;i<GKinds.entries;i++)
	{
		WriteGKind(i);
	}
}

void WriteGKind(i)
{
	PUT1(GKinds.entry[i].arity);
	PutName(GKinds.entry[i].name);
}


//////////////////////////////////////////////////////
//LKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	int arity;
}TLKind_t;

typedef struct{
	int entries;
	int size;
	TX_t* entry;
}LKind_Vec;

LKind_Vec LKinds;

void InitTLKinds()
{
	LKinds.entries=0;
	LKinds.size=128;
	LKinds.entry=malloc(LKinds.size*sizeof(TLKind_t));
	if(LKinds.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTLKinds(int count)
{
	int tmp=LKinds.entries;
	LKinds.entries=LKinds.entries+count;
	if(LKinds.entries>LKinds.size)
	{
		do{
			LKinds.size*=2;
		}while(LKinds.entries>LKinds.size)
		
			LKinds.entry=(TLKind_t*)realloc((void*)LKinds.entry,LKinds.size*sizeof(TLKind_t));
			if(LKinds.entry==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
	}
	return tmp;
}

void LoadLKinds()
{
	int count=CM.LKindcount=GET1();
	int offset=CM.LKindoffset=AllocateTLKinds(count);
	//CM.LKind=AllocateLLKinds(count);
	for(int i=0;i<count;i++)
	{
		LoadLKind(offset+i);
	}
}

void LoadLKind(int i)
{
	LKinds.entry[i].arity=GET1();
}

void WriteLKinds()
{
	PUT1(LKinds.entries);
	for(int i=0;i<LKinds.entries;i++)
	{
		WriteLKind(i);
	}
}

void WriteLKind(i)
{
	PUT1(LKinds.entry[i].arity);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
KindInd GetKindInd(){
	KindInd tmp;
	tmp.gl_flag=GET1();
	tmp.index=GET1();
	if(tmp.gl_flag==LOCAL_KIND)
	{
		tmp.index+=CM.LKindOffset;
		return tmp;
	}
	//GLOBAL_KIND
	return CM.GKind[index];
}

KindInd RenameKind(Name name)
{
	//TODO Lookup name in CM.KindRenameTable
}

int CheckKindArity(KindInd i)
{
	if(i.gl_flag==LOCAL_KIND)
	{
		return LKinds.entry[i.index].arity;
	}
	//GLOBAL_KIND
	return GKinds.entry[i.index].arity;
}
