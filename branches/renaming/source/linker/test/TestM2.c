#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/module.h"
#include "TestModule.h"
#include "TestM2.h"

void TEST_CreateM1M2KindRenameTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PutString(fd,"glob1"); LK_FILE_PUT1(fd,GLOBAL); LK_FILE_PUT2(fd,1);
  LK_FILE_PutString(fd,"glob0"); LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);
}

void TEST_CreateM2GKindTable(int fd)
{
  LK_FILE_PUT2(fd,3);
  LK_FILE_PUT1(fd,0); LK_FILE_PutString(fd,"foo");
  LK_FILE_PUT1(fd,1); LK_FILE_PutString(fd,"baz");
  LK_FILE_PUT1(fd,0); LK_FILE_PutString(fd,"bar");
}

void TEST_CreateM2LKindTable(int fd)
{
  LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,0x7c);
}

void TEST_CreateM2KindTables(int fd)
{
  TEST_CreateM2GKindTable(fd);
  TEST_CreateM2LKindTable(fd);
}

void TEST_CheckM2GKindTable(int fd)
{
  char* tmp=NULL;
  ASSERT(LK_FILE_GET2(fd)==3,"M2 - Global kind count");
  ASSERT(LK_FILE_GET1(fd)==0,"M2 - GKind0 : Arity");
  tmp=LK_FILE_GetString(fd); ASSERT(!strcmp(tmp,"foo"),"M2 - GKind0 : name"); free(tmp);
  ASSERT(LK_FILE_GET1(fd)==1,"M2 - GKind1 : Arity");
  tmp=LK_FILE_GetString(fd); ASSERT(!strcmp(tmp,"baz"),"M2 - GKind1 : name"); free(tmp);
  ASSERT(LK_FILE_GET1(fd)==0,"M2 - GKind2 : Arity");
  tmp=LK_FILE_GetString(fd); ASSERT(!strcmp(tmp,"bar"),"M2 - GKind2 : name"); free(tmp);
}

void TEST_CheckM2LKindTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==3,"M2 - Local kind count");
  ASSERT(LK_FILE_GET1(fd)==0x7c,"M2 - LKind0 : Arity");
  ASSERT(LK_FILE_GET1(fd)==1,"M2 - LKind1 : Arity");
  ASSERT(LK_FILE_GET1(fd)==0,"M2 - LKind2 : Arity");
}

void TEST_CreateM2TySkelTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,ARROW);
  LK_FILE_PUT1(fd,KIND);LK_FILE_PUT1(fd,GLOBAL);LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,VARIABLE);LK_FILE_PUT1(fd,1);
}

void TEST_CheckM2TySkelTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==4,"NumTySkels");
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

void TEST_CreateM2GConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PutString(fd,"Glob0");LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PutString(fd,"Glob1");LK_FILE_PUT2(fd,1);
}

void TEST_CreateM2LConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT2(fd,1);
}

void TEST_CreateM2HConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT2(fd,1);
}

void TEST_CreateM2ConstTables(int fd)
{
  TEST_CreateM1GConstTable(fd);
  TEST_CreateM1LConstTable(fd);
  TEST_CreateM1HConstTable(fd);
}

void TEST_CreateM1M2ConstRenameTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PutString(fd,"Glob1"); LK_FILE_PUT1(fd,GLOBAL); LK_FILE_PUT2(fd,1);
  LK_FILE_PutString(fd,"Glob0"); LK_FILE_PUT1(fd,GLOBAL); LK_FILE_PUT2(fd,0);
}

void TEST_CheckM2GConstTable(int fd)
{
  char* tmp=NULL;
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Global constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst0 : ty_env_size");
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Glob0"),"M1 - GConst0 : name");
  free(tmp);
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - GConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - GConst1 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst1 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - GConst1 : ty_env_size");
  tmp=LK_FILE_GetString(fd);
  ASSERT(!strcmp(tmp,"Glob1"),"M1 - GConst1 : name");
  free(tmp);
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - GConst1 : ty_skel_index");
}

void TEST_CheckM2LConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==4,"M2 - Local constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst0 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst0 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst0 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==1,"M2 - LConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M2 - LConst1 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst1 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst1 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==1,"M2 - LConst1 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst2 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst2 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst2 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==3,"M2 - LConst2 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M2 - LConst3 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst3 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M2 - LConst3 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==3,"M2 - LConst3 : ty_skel_index");
}

void TEST_CheckM2HConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==4,"M1 - Hidden constant count");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HConst1 : ty_skel_index");
  ASSERT(LK_FILE_GET2(fd)==3,"M1 - HConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET2(fd)==3,"M1 - HConst1 : ty_skel_index");
}

void TEST_CheckM2ConstTables(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==10,"M1 - Total Const count");
  TEST_CheckM2GConstTable(fd);
  TEST_CheckM2LConstTable(fd);
  TEST_CheckM2HConstTable(fd);
}

