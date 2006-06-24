#include <stdlib.h>
#include "module.h"
#include "vector.h"
#include "rename.h"
/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GConsts and LConsts/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GConst Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 fixity;
	INT1 precedence;
	INT1 ty_env_size;
	INT1 ty_preserving_info;
	Name name;
	INT2 ty_skel_index;
}TGConst_t;

struct Vector GConsts;

ConstInd* AllocateLGConsts(int count);
ConstInd LoadGConst();
void WriteGConst(int i);
ConstInd LoadTopGConst(int i);

void InitTGConsts()
{
	InitVec(&GConsts,128,sizeof(TGConst_t));
}

ConstInd* AllocateLGConsts(int count)
{
	ConstInd* tmp=(ConstInd*)malloc(count*sizeof(ConstInd));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return tmp;
}

void LoadGConsts()
{
	int i;
	int count=CM->GConstcount=GET2();
	CM->GConst=AllocateLGConsts(count);
	for(i=0;i<count;i++)
	{
		CM->GConst[i]=LoadGConst();
	}
}

ConstInd LoadGConst()
{
	TGConst_t tmp;
	tmp.fixity=GET1();
	tmp.precedence=GET1();
	tmp.ty_env_size=GET1();;
	tmp.ty_preserving_info=GET1();
	Name name;
	GetName(&name);
	ConstInd index=RenameConst(name);
	tmp.ty_skel_index=GetTySkelInd();
	if(!CheckGConstEqv(index,tmp))
	{
		perror("Constant parameter mismatch");
		exit(0);
	}
	return index;
}

void LoadTopGConsts()
{
	int i;
	int count=CM->GConstcount=GET2();
	Extend(&GConsts,count);
	CM->GConst=AllocateLGConsts(count);
	for(i=0;i<count;i++)
	{
		CM->GConst[i]=LoadTopGConst(i);
	}
}

ConstInd LoadTopGConst(int i)
{
	ConstInd index;
	index.index=i;
	index.gl_flag=GLOBAL;
	TGConst_t* tmp=(TGConst_t*)Fetch(&GConsts,i);
	
	tmp->fixity=GET1();
	tmp->precedence=GET1();
	tmp->ty_env_size=GET1();;
	tmp->ty_preserving_info=GET1();
	GetName(&(tmp->name));
	tmp->ty_skel_index=GetTySkelInd();
	
	return index;
}

void WriteGConsts()
{
	int i;
	PUT2(GConsts.numEntries);
	for(i=0;i<GConsts.numEntries;i++)
	{
		WriteGConst(i);
	}
}

void WriteGConst(i)
{
	TGConst_t* tmp=(TGConst_t*)Fetch(&GConsts,i);
	PUT1(tmp->fixity);
	PUT1(tmp->precedence);
	PUT1(tmp->ty_env_size);
	PUT1(tmp->ty_preserving_info);
	PutName(tmp->name);
	PUT2(tmp->ty_skel_index);
}


//////////////////////////////////////////////////////
//LConst Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 fixity;
	INT1 precedence;
	INT1 ty_env_size;
	INT2 ty_skel_index;
}TLConst_t;

struct Vector LConsts;

void LoadLConst(int i);
void WriteLConst(int i);

void InitTLConsts()
{
	InitVec(&LConsts,128,sizeof(TLConst_t));
}

void LoadLConsts()
{
	int i;
	int count=CM->LConstcount=GET2();
	int offset=CM->LConstoffset=Extend(&LConsts,count);
	for(i=0;i<count;i++)
	{
		LoadLConst(offset+i);
	}
}

void LoadLConst(int i)
{
	TLConst_t* tmp=(TLConst_t*)Fetch(&LConsts,i);
	tmp->fixity=GET1();
	tmp->precedence=GET1();
	tmp->ty_env_size=GET1();
	tmp->ty_skel_index=GetTySkelInd();
}

void WriteLConsts()
{
	int i;
	PUT2(LConsts.numEntries);
	for(i=0;i<LConsts.numEntries;i++)
	{
		WriteLConst(i);
	}
}

void WriteLConst(i)
{
	TLConst_t* tmp=(TLConst_t*)Fetch(&LConsts,i);
	PUT1(tmp->fixity);
	PUT1(tmp->precedence);
	PUT1(tmp->ty_env_size);
	PUT2(tmp->ty_skel_index);
}

//////////////////////////////////////////////////////
//HConst Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 ty_env_size;
	INT2 ty_skel_index;
}THConst_t;

struct Vector HConsts;

void LoadHConst(int i);
void WriteHConst(int i);

void InitTHConsts()
{
	InitVec(&HConsts,128,sizeof(THConst_t));
}

void LoadHConsts()
{
	int i;
	int count=CM->HConstcount=GET2();
	int offset=CM->HConstoffset=Extend(&HConsts,count);
	for(i=0;i<count;i++)
	{
		LoadHConst(offset+i);
	}
}

void LoadHConst(int i)
{
	THConst_t* tmp=(THConst_t*)Fetch(&HConsts,i);
	
	tmp->ty_env_size=GET1();
	tmp->ty_skel_index=GetTySkelInd();
}

void WriteHConsts()
{
	int i;
	PUT2(HConsts.numEntries);
	for(i=0;i<HConsts.numEntries;i++)
	{
		WriteHConst(i);
	}
}

void WriteHConst(i)
{
	THConst_t* tmp=(THConst_t*)Fetch(&HConsts,i);
	
	PUT1(tmp->ty_env_size);
	PUT2(tmp->ty_skel_index);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckGConstEqv(ConstInd i,TGConst_t new)
{
	int b=1;
	if(i.gl_flag==LOCAL)
	{
		TLConst_t* tmp=(TLConst_t*)Fetch(&LConsts,i.index);
	
		b=b&&tmp->fixity==new.fixity;
		b=b&&tmp->precedence==new.precedence;
		b=b&&tmp->ty_env_size==new.ty_env_size;
		b=b&&0==TySkelCmp(tmp->ty_skel_index,new.ty_skel_index);
		return b;
	}
	//GLOBAL
	TGConst_t* tmp2=(TGConst_t*)Fetch(&GConsts,i.index);
	
	b=b&&tmp2->fixity==new.fixity;
	b=b&&tmp2->precedence==new.precedence;
	b=b&&tmp2->ty_env_size==new.ty_env_size;
	b=b&&tmp2->ty_preserving_info==new.ty_preserving_info;
	b=b&&0==TySkelCmp(tmp2->ty_skel_index,new.ty_skel_index);
	return b;
}
