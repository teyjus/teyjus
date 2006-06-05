#include <stdlib.h>
#include "module.h"
#include "tyskel.h"
#include "kind.h"
#include "file.h"

//////////////////////////////////////////////////////
//TySkel Load and Write Code
//////////////////////////////////////////////////////

struct TTySkel_st{
	INT1 type;
	INT2 value;
	struct TTySkel_st* args;
};

typedef struct TTySkel_st TTySkel_t;
#define ARROW 0
#define PKIND 1
#define LKIND 2
#define GKIND 3
#define VARIABLE 4

typedef struct{
	int entries;
	int size;
	TTySkel_t* entry;
}TySkel_Vec;

TySkel_Vec TySkels;

void InitTTySkels();
TTySkel_t GetTySkel();
int AllocateTTySkels(int count);
void LoadTySkel(int i);
void LoadTySkels();
TTySkel_t GetTySkel();
void WriteTySkels();
void WriteTySkel(int i);
void PutTySkel(TTySkel_t tyskel);



void InitTTySkels()
{
	TySkels.entries=0;
	TySkels.size=128;
	TySkels.entry=malloc(TySkels.size*sizeof(TTySkel_t));
	if(TySkels.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTTySkels(int count)
{
	int tmp=TySkels.entries;
	TySkels.entries=TySkels.entries+count;
	if(TySkels.entries>TySkels.size)
	{
		do{
			TySkels.size*=2;
		}while(TySkels.entries>TySkels.size);
		
		TySkels.entry=(TTySkel_t*)realloc((void*)TySkels.entry,TySkels.size*sizeof(TTySkel_t));
		if(TySkels.entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

// LTySkel_t* AllocateLTySkels(int count)
// {
// 	LTySkel_t* tmp=(LTySkel_t*)malloc(count*sizeof(LTySkel_t));
// 	if(tmp==NULL)
// 	{
// 		perror("Memory Allocation Failed");
// 		exit(0);
// 	}
// 	
// 	return tmp;
// }

void LoadTySkel(int i)
{
	TySkels.entry[i]=GetTySkel();
}

void LoadTySkels()
{
	int i;
	int count=CM->TySkelcount=GET1();
	int offset=CM->TySkeloffset=AllocateTTySkels(count);
	//CM->TySkel=AllocateLTySkels(count);
	for(i=0;i<count;i++)
	{
		LoadTySkel(offset+i);
	}
}

TTySkel_t GetTySkel()
{
	int i;
	TTySkel_t tmp;
	tmp.args=NULL;
	tmp.type=GET1();
	switch(tmp.type)
	{
		case ARROW:
			tmp.value=(INT2)GET1();
			tmp.args=malloc(tmp.value*sizeof(TTySkel_t));
			if(tmp.args==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
			for(i=0;i<tmp.value;i++)
			{
				tmp.args[i]=GetTySkel();
			}
			break;
		
		case PKIND:
			tmp.value=GET2();
			break;
			
		case LKIND:
			tmp.value=GetLKindInd();
			break;
			
		case GKIND:
			tmp.value=GetGKindInd();
			break;
			
		case VARIABLE:
			tmp.value=(INT2)GET1();
			break;
			
		default:
			perror("Error, Invalid Type Head");
			exit(0);
	}
	return tmp;
}

void WriteTySkels()
{
	int i;
	PUT1(TySkels.entries);
	for(i=0;i<TySkels.entries;i++)
	{
		WriteTySkel(i);
	}
}

void WriteTySkel(int i)
{
	PutTySkel(TySkels.entry[i]);
}

void PutTySkel(TTySkel_t tyskel)
{
	INT1 x=tyskel.type;
	int i;
	PUT1(x);
	switch(x)
	{
		case ARROW:
			PUT1((INT1)tyskel.value);
			for(i=0;i<tyskel.value;i++)
			{
				PutTySkel(tyskel.args[i]);
			}
			break;
		
		case PKIND:
		case LKIND:
		case GKIND:
			PUT2(tyskel.value);
			break;
			
		case VARIABLE:
			PUT1((INT1)tyskel.value);
			break;
			
		default:
			perror("Error, Invalid Type Head");
			exit(0);
	}
}
