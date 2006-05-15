#include "toplevel.h"
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

typedef struct{
	int entries;
	int size;
	TX_t* entry;
}GConst_Vec;

GConst_Vec GConsts;

void InitTGConsts()
{
	GConsts.entries=0;
	GConsts.size=0;
	GConsts.entry=NULL;
}

int AllocateTGConsts(int count)
{
	GConsts.entries=count;
	GConsts.size=count;
	malloc(count*sizeof(TGConst_t));
	if(GConsts.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return 0;
}

LGConst_t* AllocateLGConsts(int count)
{
	LGConst_t* tmp=(LGConst_t*)malloc(count*sizeof(LGConst_t));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return tmp;
}

void LoadGConsts()
{
	int count=CM.GConstcount=GET1();
	//int offset=CM.GConstoffset=AllocateTGConsts(count);
	CM.GConst=AllocateLGConsts(count);
	for(int i=0;i<count;i++)
	{
		CM.GConst[i]=LoadGConst();
	}
}

LGConst_t LoadGConst()
{
	TGConst_t tmp;
	tmp.fixity=GET1();
	tmp.precedence=GET1();
	tmp.ty_env_size=GET1();;
	tmp.ty_preserving_info=GET1();
	ConstInd index=RenameConst(GetName());
	tmp.ty_skel_index=GET2()+TySkeloffset;
	if(!CheckGConstEqv(index,tmp))
	{
		perror("Constant parameter mismatch");
		exit(0);
	}
	return (LGConst_t)index;
}

void LoadTopGConsts()
{
	int count=CM.GConstcount=GET1();
	AllocateTGConsts(count);
	CM.GConst=AllocateLGConsts(count);
	for(int i=0;i<count;i++)
	{
		CM.GConst[i]=LoadTopGConst(i);
	}
}

LGConst_t LoadTopGConst(int i)
{
	LGConst_t tmp;
	tmp.index.index=i;
	tmp.index.gl_flag=GLOBAL_CONST;

	GConst_Vec.entry[i].fixity=GET1();
	GConst_Vec.entry[i].precedence=GET1();
	GConst_Vec.entry[i].ty_env_size=GET1();;
	GConst_Vec.entry[i].ty_preserving_info=GET1();
	GConst_Vec.entry[i].name=GetName();
	GConst_Vec.entry[i].ty_skel_index=GET2()+TySkeloffset;
	
	return tmp;
}

void WriteGConsts()
{
	PUT1(GConsts.entries);
	for(int i=0;i<GConsts.entries;i++)
	{
		WriteGConst(i);
	}
}

void WriteGConst(i)
{
	PUT1(GConst_Vec.entry[i].fixity);
	PUT1(GConst_Vec.entry[i].precedence);
	PUT1(GConst_Vec.entry[i].ty_env_size);
	PUT1(GConst_Vec.entry[i].ty_preserving_info);
	PutName(GConst_Vec.entry[i].name);
	PUT2(GConst_Vec.entry[i].ty_skel_index);
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

typedef struct{
	int entries;
	int size;
	TX_t* entry;
}LConst_Vec;

LConst_Vec LConsts;

void InitTLConsts()
{
	LConsts.entries=0;
	LConsts.size=128;
	LConsts.entry=malloc(LConsts.size*sizeof(TLConst_t));
	if(LConsts.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTLConsts(int count)
{
	int tmp=LConsts.entries;
	LConsts.entries=LConsts.entries+count;
	if(LConsts.entries>LConsts.size)
	{
		do{
			LConsts.size*=2;
		}while(LConsts.entries>LConsts.size)
		
			LConsts.entry=(TLConst_t*)realloc((void*)LConsts.entry,LConsts.size*sizeof(TLConst_t));
			if(LConsts.entry==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
	}
	return tmp;
}

void LoadLConsts()
{
	int count=CM.LConstcount=GET1();
	int offset=CM.LConstoffset=AllocateTLConsts(count);
	//CM.LConst=AllocateLLConsts(count);
	for(int i=0;i<count;i++)
	{
		LoadLConst(offset+i);
	}
}

void LoadLConst(int i)
{
	LConst_Vec.entry[i].fixity=GET1();
	LConst_Vec.entry[i].precedence=GET1();
	LConst_Vec.entry[i].ty_env_size=GET1();
	LConst_Vec.entry[i].ty_skel_index=GET2()+CM.TySkeloffset;
}

void WriteLConsts()
{
	PUT1(LConsts.entries);
	for(int i=0;i<LConsts.entries;i++)
	{
		WriteLConst(i);
	}
}

void WriteLConst(i)
{
	PUT1(LConsts.entry[i].fixity);
	PUT1(LConsts.entry[i].precedence);
	PUT1(LConsts.entry[i].ty_env_size);
	PUT2(LConsts.entry[i].ty_skel_index);
}

//////////////////////////////////////////////////////
//HConst Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 ty_env_size;
	INT2 ty_skel_index;
}THConst_t;

typedef struct{
	int entries;
	int size;
	TX_t* entry;
}HConst_Vec;

HConst_Vec HConsts;

void InitTHConsts()
{
	HConsts.entries=0;
	HConsts.size=128;
	HConsts.entry=malloc(HConsts.size*sizeof(THConst_t));
	if(HConsts.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTHConsts(int count)
{
	int tmp=HConsts.entries;
	HConsts.entries=HConsts.entries+count;
	if(HConsts.entries>HConsts.size)
	{
		do{
			HConsts.size*=2;
		}while(HConsts.entries>HConsts.size)
		
			HConsts.entry=(THConst_t*)realloc((void*)HConsts.entry,HConsts.size*sizeof(THConst_t));
			if(HConsts.entry==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
	}
	return tmp;
}

void LoadHConsts()
{
	int count=CM.HConstcount=GET1();
	int offset=CM.HConstoffset=AllocateTHConsts(count);
	//CM.HConst=AllocateLHConsts(count);
	for(int i=0;i<count;i++)
	{
		LoadHConst(offset+i);
	}
}

void LoadHConst(int i)
{
	HConst_Vec.entry[i].ty_env_size=GET1();
	HConst_Vec.entry[i].ty_skel_index=GET2()+CM.TySkeloffset;
}

void WriteHConsts()
{
	PUT1(HConsts.entries);
	for(int i=0;i<HConsts.entries;i++)
	{
		WriteHConst(i);
	}
}

void WriteHConst(i)
{
	PUT1(HConsts.entry[i].ty_env_size);
	PUT2(HConsts.entry[i].ty_skel_index);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
ConstInd GetConstInd(){
	ConstInd tmp;
	tmp.gl_flag=GET1();
	tmp.index=GET1();
	if(tmp.gl_flag==LOCAL_CONST)
	{
		tmp.index+=CM.LConstOffset;
		return tmp;
	}
	else if(tmp.gl_flag==HIDDEN_CONST)
	{
		tmp.index+=CM.HConstOffset;
		return tmp;
	}
	else if(tmp.gl_flag==GLOBAL_CONST)
	{
		return CM.GConst[index];
	}
	//PERVASIVE_CONST
	return tmp;
}

ConstInd RenameConst(Name name)
{
	//TODO Lookup name in CM.ConstRenameTable
}

int CheckGConstEqv(ConstInd i,TGConst_t new)
{
	bool b=1;
	if(i.gl_flag==LOCAL_CONST)
	{
		b=b&&LConsts.entry[i.index].fixity==new.fixity;
		b=b&&LConsts.entry[i.index].precedence==new.precedence;
		b=b&&LConsts.entry[i.index].ty_env_size==new.ty_env_size;
		b=b&&TySkelCmp(LConsts.entry[i.index].ty_skel_index,new.ty_skel_index);
		return b;
	}
	//GLOBAL_CONST
	b=b&&GConsts.entry[i.index].fixity==new.fixity;
	b=b&&GConsts.entry[i.index].precedence==new.precedence;
	b=b&&GConsts.entry[i.index].ty_env_size==new.ty_env_size;
	b=b&&GConsts.entry[i.index].ty_preserving_info==ty_preserving_info;
	b=b&&TySkelCmp(GConsts.entry[i.index].ty_skel_index,new.ty_skel_index);
	return b
}
