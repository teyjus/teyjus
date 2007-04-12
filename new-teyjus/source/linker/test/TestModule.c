#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/module.h"
#include "TestModule.h"

char* DBG(char* str)
{
  printf("Debug(%s)\n",str);
  return str;
}

void TEST_CreateM1GKindTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,0x7c); LK_FILE_PutString(fd,"glob0");
  LK_FILE_PUT1(fd,1); LK_FILE_PutString(fd,"glob1");
}

void TEST_CreateM1LKindTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,0);
}

void TEST_CreateM1KindTables(int fd)
{
  TEST_CreateM1GKindTable(fd);
  TEST_CreateM1LKindTable(fd);
}

void TEST_CheckM1GKindTable(int fd)
{
  char* tmp=NULL;
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Global kind count");
  ASSERT(LK_FILE_GET1(fd)==0x7c,"M1 - GKind0 : Arity");
  tmp=LK_FILE_GetString(fd); ASSERT(!strcmp(tmp,"glob0"),"M1 - GKind0 : name"); free(tmp);
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - GKind1 : Arity");
  tmp=LK_FILE_GetString(fd); ASSERT(!strcmp(tmp,"glob1"),"M1 - GKind1 : name"); free(tmp);
}

void TEST_CheckM1LKindTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Local kind count");
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - LKind0 : Arity");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - LKind1 : Arity");
}

void TEST_CheckM1KindTables(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==4,"M1 - Total Kind count");
  TEST_CheckM1GKindTable(fd);
  TEST_CheckM1LKindTable(fd);
}

void TEST_CreateM1TySkelTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,ARROW);
  LK_FILE_PUT1(fd,KIND);LK_FILE_PUT1(fd,GLOBAL);LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
}

void TEST_CheckM1TySkelTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"NumTySkels");
  ASSERT(LK_FILE_GET1(fd)==ARROW,"->");
  ASSERT(LK_FILE_GET1(fd)==KIND,"K");
  ASSERT(LK_FILE_GET1(fd)==GLOBAL,"G");
  ASSERT(LK_FILE_GET2(fd)==1,"1");
  ASSERT(LK_FILE_GET1(fd)==VARIABLE,"V");
  ASSERT(LK_FILE_GET1(fd)==1,"1");
  ASSERT(LK_FILE_GET1(fd)==VARIABLE,"V");
  ASSERT(LK_FILE_GET1(fd)==1,"1");
  
  ASSERT(LK_FILE_GET1(fd)==VARIABLE,"V");
  ASSERT(LK_FILE_GET1(fd)==1,"1");
}

void TEST_CreateM1GConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT1(fd,3); LK_FILE_PutString(fd,"Glob0");LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT1(fd,3); LK_FILE_PutString(fd,"Glob1");LK_FILE_PUT2(fd,1);
}

void TEST_CreateM1LConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT1(fd,3);LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT1(fd,3);LK_FILE_PUT2(fd,1);
}

void TEST_CreateM1HConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT2(fd,1);
}

void TEST_CreateM1ConstTables(int fd)
{
  TEST_CreateM1GConstTable(fd);
  TEST_CreateM1LConstTable(fd);
  TEST_CreateM1HConstTable(fd);
}

void TEST_CheckM1GConstTable(int fd)
{
  char* tmp=NULL;
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Global constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : neededness");
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Glob0"),"M1 - GConst0 : name");
  free(tmp);
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - GConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - GConst1 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst1 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst1 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst1 : neededness");
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Glob1"),"M1 - GConst1 : name");
  free(tmp);
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - GConst1 : ty_skel_index");
}

void TEST_CheckM1LConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Local constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : neededness");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - LConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - LConst1 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst1 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst1 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst1 : neededness");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - LConst1 : ty_skel_index");
}

void TEST_CheckM1HConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Hidden constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - HConst0 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - HConst0 : neededness");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - HConst1 : ty_env_size");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - HConst1 : neededness");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HConst1 : ty_skel_index");
}

void TEST_CheckM1ConstTables(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==6,"M1 - Total Const count");
  TEST_CheckM1GConstTable(fd);
  TEST_CheckM1LConstTable(fd);
  TEST_CheckM1HConstTable(fd);
}

void TEST_CreateM1StringTable(int fd)
{
  LK_FILE_PUT2(fd,4);
  LK_FILE_PutString(fd,"A String");
  LK_FILE_PutString(fd,"Another String");
  LK_FILE_PutString(fd,"YAS");
  LK_FILE_PutString(fd,"Yet Another String");
}

void TEST_CheckM1StringTable(int fd)
{
  char* tmp=NULL;
  ASSERT(LK_FILE_GET2(fd)==4,"M1 - Strings count");
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"A String"),"M1 - String0");
  free(tmp);
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Another String"),"M1 - String1");
  free(tmp);
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"YAS"),"M1 - String2");
  free(tmp);
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Yet Another String"),"M1 - String3");
  free(tmp);
}
