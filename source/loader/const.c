#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "const.h"

//NUM_PERVASIVE_CONSTS
#define NUM_PERVASIVE_CONSTS 10

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

TwoBytes LD_CONST_numGConsts;
TwoBytes LD_CONST_numLConsts;

int LD_CONST_LoadCst(MEM_GmtEnt* ent)
{
	int i;
	
	TwoBytes cstsize=LD_FILE_GET2();
	MEM_CstEnt* cst=(MEM_CstEnt*)LD_LOADER_ExtendModSpace(ent,(cstsize+NUM_PERVASIVE_CONSTS)*sizeof(MEM_CstEnt));
	
	//Copy Pervasive constants.
	cst+=NUM_PERVASIVE_CONSTS;
	
	TwoBytes num_glob=LD_CONST_numGConsts=LD_FILE_GET2();
	if(num_glob>cstsize)
		return -1;
	
	int str_length;
	char* string;
	for(i=0;i<num_glob;i++)
	{
		cst[i].fixity=LD_FILE_GET1();
		cst[i].precedence=LD_FILE_GET1();
		cst[i].typeEnvSize=LD_FILE_GET1();
		//TODO TYPEPRESINFO/NEEDNESS VECTOR
		cst[i].univCount=0;
		
		str_length=LD_FILE_GET1();
		string=(char*)LD_LOADER_ExtendModSpace(ent,str_length);
		LD_FILE_GetString(string,str_length);
		cst[i].name=string;
		
		cst[i].tskTabIndex=LD_FILE_GET2();
	}
	
	TwoBytes num_loc=LD_CONST_numLConsts=LD_FILE_GET2();
	if(num_glob+num_loc>cstsize)
		return -1;
	cst+=num_glob;
	for(i=0;i<num_loc;i++)
	{
		cst[i].fixity=LD_FILE_GET1();
		cst[i].precedence=LD_FILE_GET1();
		cst[i].typeEnvSize=LD_FILE_GET1();
		cst[i].univCount=0;
		cst[i].name=NULL;
		cst[i].tskTabIndex=LD_FILE_GET2();
	}
	
	TwoBytes num_hid=LD_FILE_GET2();
	if(num_glob+num_loc+num_hid!=cstsize)
		return -1;
	cst+=num_loc;
	for(i=0;i<num_hid;i++)
	{
		cst[i].fixity=0;
		cst[i].precedence=0;
		cst[i].typeEnvSize=LD_FILE_GET1();
		cst[i].univCount=0;
		cst[i].name=NULL;
		cst[i].tskTabIndex=LD_FILE_GET2();
	}
	return 0;
}

TwoBytes LD_CONST_GetConstInd()
{
	Byte gl=LD_FILE_GET1();
	TwoBytes ind=LD_FILE_GET2();
	switch(gl)
	{
		case HIDDEN:
			ind+=LD_CONST_numLConsts;
		case LOCAL:
			ind+=LD_CONST_numGConsts;
		case GLOBAL:
			ind+=NUM_PERVASIVE_CONSTS;
		case PERVASIVE:
			return ind;
			break;
		default:
			//TODO handle error;
			return -1;
			break;
	}
}
