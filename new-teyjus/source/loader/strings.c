#include "strings.h"
#include "loader.h"
#include "file.h"
#include "../system/memory.h"

TwoBytes LD_LOADER_numStrings;
char** LD_LOADER_Strings;

void LD_STRING_LoadStrings(MEM_GmtEnt* ent)
{
	int i;
	char* string;
	int str_length;
	TwoBytes count=LD_LOADER_numStrings=LD_FILE_GET2();
	LD_LOADER_Strings=(char**)malloc(count*sizeof(char*));
	for(i=0;i<count;i++)
	{
		str_length=LD_FILE_GET1();
		string=LD_LOADER_Strings[i]=(char*)LD_LOADER_ExtendModSpace(ent,str_length);
		LD_FILE_GetString(string,str_length);
		printf("Loaded string \"%s\"\n",string);
	}
	return;
}
