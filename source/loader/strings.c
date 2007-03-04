#include "strings.h"
#include "loader.h"
#include "file.h"
#include "../system/memory.h"
#include "../simulator/mcstring.h"

TwoBytes LD_LOADER_numStrings;
DF_StrDataPtr* LD_LOADER_Strings;

void LD_STRING_LoadStrings(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_LOADER_numStrings=LD_FILE_GET2();
  LD_LOADER_Strings=(DF_StrDataPtr*)EM_malloc(count*sizeof(char*));
  
  for(i=0;i<count;i++)
  {
    LD_LOADER_Strings[i]= LD_STRING_LoadString(ent);
  }
  return;
}

DF_StrDataPtr LD_STRING_LoadString(MEM_GmtEnt* ent)
{
  int str_length=LD_FILE_GET1();
  char* string=(char*)EM_malloc((str_length+1)*sizeof(char));
  LD_FILE_GetString(string,str_length);
  
  DF_StrDataPtr loc=(DF_StrDataPtr)LD_LOADER_ExtendModSpace(ent,MCSTR_numWords(str_length)+DF_STRDATA_HEAD_SIZE);
  
  DF_mkStrDataHead((MemPtr)loc);
  MCSTR_toString((MCSTR_Str)(loc+DF_STRDATA_HEAD_SIZE), string, str_length);
  
  free(string);
  return loc;
}

DF_StrDataPtr LD_STRING_GetStringAddr(int i)
{
  return LD_LOADER_Strings[i];
}

void LD_STRING_Cleanup()
{
  if(LD_LOADER_Strings)
    free(LD_LOADER_Strings);
}
