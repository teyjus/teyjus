
#include <stdlib.h>
#include "stringspace.h"
#include "module.h"
#include "vector.h"
#include "file.h"


#include <stdio.h>
#define DEBUG(x) printf("%s\n",x)
//////////////////////////////////////////////////////
//StringSpace Load and Write Code
//////////////////////////////////////////////////////
struct Vector Strings;

void LK_STRINGS_Init()
{
  LK_VECTOR_Init(&Strings,128,sizeof(char*));
}

void LoadString(int fd, struct Module_st* CMData,void* entry)
{
  *(char**)entry=LK_FILE_GetString(fd);
}

void LK_STRINGS_Load(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&Strings,CMData,&(CMData->StringsAdj),LoadString);
}

void WriteString(int fd, void* ent)
{
  char* tmp=*(char**)ent;
  LK_FILE_PutString(fd,tmp);
  free(tmp);
}

void LK_STRINGS_Write(int fd)
{
  LK_VECTOR_Write(fd, &Strings,&WriteString);
  LK_VECTOR_Free(&Strings);
}
