#include <stdio.h>
#include "file.h"
#include "../simulator/dataformats.h"
#include "loader.h"
#include "tyskel.h"
#include "kind.h"

#define ARROW 0
#define PERVASIVE 1
#define LOCAL 2
#define GLOBAL 3
#define VARIABLE 4

int LD_TYSKEL_LoadType(MEM_GmtEnt* ent,DF_TypePtr loc);

int LD_TYSKEL_LoadTst(MEM_GmtEnt* ent)
{
	int i;
	TwoBytes tstsize=LD_FILE_GET2();
	DF_TypePtr* tst=(DF_TypePtr*)LD_LOADER_ExtendModSpace(ent,tstsize*sizeof(DF_TypePtr));
	
	for(i=0;i<tstsize;i++)
	{
		tst[i]=(DF_TypePtr)LD_LOADER_ExtendModSpace(ent,sizeof(DF_Type));
		printf("Type#%d:",i); //DEBUG
		if(-1==LD_TYSKEL_LoadType(ent,tst[i]))
			return -1;
		printf(":\n"); //DEBUG
	}
	return 0;
}

int LD_TYSKEL_LoadType(MEM_GmtEnt* ent,DF_TypePtr loc)
{
	DF_TypePtr args=NULL;
	Byte type=LD_FILE_GET1();
	switch(type)
	{
		case ARROW:
			args=(DF_TypePtr)LD_LOADER_ExtendModSpace(ent,2*sizeof(DF_Type));
			DF_mkArrowType((MemPtr)loc,args);
			LD_TYSKEL_LoadType(ent,args);
			DEBUG("->");
			LD_TYSKEL_LoadType(ent,args+1);
			break;
			
		case PERVASIVE:
		case LOCAL:
		case GLOBAL:
			DF_mkSortType((MemPtr)loc,(int)LD_KIND_ConvKindInd(type,LD_FILE_GET2()));
			DEBUG("K");
			break;
			
		case VARIABLE:
		default:
			DF_mkSkelVarType((MemPtr)loc,(int)LD_FILE_GET1());
			DEBUG("V");
			break;
	}
	
	return 0;
}


