#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "kind.h"

#define NUM_PERVASIVE_KINDS 10 //Fixme
#define GLOBAL 0
#define LOCAL 1

TwoBytes LD_KIND_numGKinds=-1;

int LD_KIND_LoadKst(MEM_GmtEnt* ent)
{
	int i;
	char str_length;
	char* string=NULL;
	
	//Allocate space for the kind table.
	TwoBytes kstsize=LD_FILE_GET2();
	MEM_KstEnt* kst=(MEM_KstEnt*)LD_LOADER_ExtendModSpace(ent,(kstsize+NUM_PERVASIVE_KINDS)*sizeof(MEM_KstEnt));
	ent->kstBase=(MEM_KstPtr)kst;
	
	//Copy the pervasive kinds
	//TODO load pervasives.
	kst+=NUM_PERVASIVE_KINDS;
			
	//Get the number of global kinds
	TwoBytes num_glob=LD_KIND_numGKinds=LD_FILE_GET2();
	if(num_glob>kstsize)
		return -1;
	
	//Load the global kinds
	for(i=0;i<num_glob;i++)
	{
		kst[i].arity=LD_FILE_GET1();
		str_length=LD_FILE_GET1();
		string=(char*)LD_LOADER_ExtendModSpace(ent,str_length);
		LD_FILE_GetString(string,str_length);
		kst[i].name=string;
	}
	
	//Load the local kinds
	TwoBytes num_loc=LD_FILE_GET2();
	if(num_glob+num_loc!=kstsize)
		return -1;
	kst+=num_glob;
	for(i=0;i<num_loc;i++)
	{
		kst[i].arity=LD_FILE_GET1();
		kst[i].name=NULL;
	}
	
	return 0;
}

TwoBytes LD_KIND_ConvKindInd(Byte gl, TwoBytes ind)
{
	if(gl==3)
		return ind+NUM_PERVASIVE_KINDS;
	else if(gl==2)
		return ind+NUM_PERVASIVE_KINDS+LD_KIND_numGKinds;
	else
		return ind;
}

TwoBytes LD_KIND_GetKindInd()
{
	Byte gl=LD_FILE_GET1();
	TwoBytes ind=LD_FILE_GET2();
	if(gl==GLOBAL)
		return ind+NUM_PERVASIVE_KINDS;
	else if(gl==LOCAL)
		return ind+NUM_PERVASIVE_KINDS+LD_KIND_numGKinds;
	else
		return ind;
}
