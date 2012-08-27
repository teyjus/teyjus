#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/module.h"
#include "TestModule.h"
#include "tables/instructions.h"

char* DBG(char* str)
{
  printf("Debug(%s)\n",str);
  return str;
}

int DBGI(int i)
{
  printf("Debug(%d)\n",i);
  return i;
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
  LK_FILE_PutString(fd,"Glob0");LK_FILE_PUT2(fd,1);
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PutString(fd,"Glob1");LK_FILE_PUT2(fd,1);
}

void TEST_CreateM1LConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT1(fd,0);LK_FILE_PUT1(fd,3);LK_FILE_PUT1(fd,3);
  LK_FILE_PUT2(fd,1);
}

void TEST_CreateM1HConstTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  
  LK_FILE_PUT2(fd,1);
  
  LK_FILE_PUT2(fd,1);
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

void TEST_CheckM1LConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Local constant count");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst0 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - LConst0 : ty_skel_index");
  ASSERT(LK_FILE_GET1(fd)==0,"M1 - LConst1 : fixity");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst1 : precedence");
  ASSERT(LK_FILE_GET1(fd)==3,"M1 - LConst1 : ty_env_size");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - LConst1 : ty_skel_index");
}

void TEST_CheckM1HConstTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - Hidden constant count");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HConst0 : ty_skel_index");
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

const int M1CodeSize=0x200;

void TEST_CreateM1CodeSize(int fd)
{
  LK_FILE_PUTWord(fd,(Word)M1CodeSize);
}

void TEST_CreateM1ImplGoalTable(int fd)
{
  LK_FILE_PUT2(fd,1);//count
  LK_FILE_PUT2(fd,1);//nctsize
  LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);  
  LK_FILE_PUT1(fd,1);//fcf
  LK_FILE_PUT2(fd,1);//nop
  LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);  
  LK_FILE_PUTWord(fd,(Word)0x100);
}

void TEST_CheckM1ImplGoalTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - ImplGoal count");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - ImplGoal1 - NCT Size");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - ImplGoal1 - NCT - L0");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - ImplGoal1 - NCT - L0");
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - ImplGoal1 - FCF");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - ImplGoal1 - NOP");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - ImplGoal1 - ST - L0");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - ImplGoal1 - ST - L0");
  ASSERT(LK_FILE_GETWord(fd)==(Word)0x100,"M1 - ImplGoal1 - ST - L0");
}

void TEST_CreateM1HashTabs(int fd)
{
  LK_FILE_PUT2(fd,1);
  LK_FILE_PUT2(fd,1);//nop
  LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);  
  LK_FILE_PUTWord(fd,(Word)0x100);
}

void TEST_CheckM1HashTabs(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HashTab count");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - HashTab1 - NOP");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - HashTab1 - L0");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - HashTab1 - L0");
  ASSERT(LK_FILE_GETWord(fd)==(Word)0x100,"M1 - HashTab1 - L0");
}

void TEST_CreateM1BvrTabs(int fd)
{
  LK_FILE_PUT2(fd,1);
  LK_FILE_PUT2(fd,1);//nop
  LK_FILE_PUT1(fd,1);
  LK_FILE_PUTWord(fd,(Word)0x100);
}

void TEST_CheckM1BvrTabs(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - BvrTab count");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - BvrTab1 - NOP");
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - BvrTab1 - Index1 id");
  ASSERT(LK_FILE_GETWord(fd)==(Word)0x100,"M1 - HashTab1 - Index1 addr");
}

void TEST_CreateM1ImportTable(int fd)
{
  LK_FILE_PUT2(fd,0);//nctsize
  LK_FILE_PUT2(fd,0);//nexportdefs
  LK_FILE_PUT2(fd,1);//nlocalpreds
  LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);
  LK_FILE_PUT1(fd,1);//fcf
  LK_FILE_PUT2(fd,1);//nop
  LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,0);
  LK_FILE_PUTWord(fd,(Word)0x100);
}

void TEST_CheckM1ImportTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==1,"Import tab count");
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - IT - NumSegs");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - IT - NCTSize");
  ASSERT(LK_FILE_GET2(fd)==2,"M1 - IT - Local Consts");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - IT - Local Consts - L0");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - IT - Local Consts - L0");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - IT - Local Consts - L1");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - IT - Local Consts - L1");
  ASSERT(LK_FILE_GET1(fd)==1,"M1 - IT - FCF");
  ASSERT(LK_FILE_GET2(fd)==1,"M1 - IT - NOP");
  ASSERT(LK_FILE_GET1(fd)==LOCAL,"M1 - IT - ST - L0");
  ASSERT(LK_FILE_GET2(fd)==0,"M1 - IT - ST - L0");
  ASSERT(LK_FILE_GETWord(fd)==(Word)0x100,"M1 - IT - ST - L0");
}

void TEST_CreateM1Code(int fd)
{
  int i=0;
  LK_FILE_PUT1(fd,execute);
  LK_FILE_PUT1(fd,LOCAL);
  LK_FILE_PUT2(fd,0);
  i+=INSTR_instrSize(execute)*sizeof(Word);
  while(i<M1CodeSize)
  {
    LK_FILE_PUT1(fd,fail);
    i+=INSTR_instrSize(fail)*sizeof(Word);
  }
}

void TEST_CheckM1Code(int fd)
{
  int i=0;
  ASSERT(execute==LK_FILE_GET1(fd),"M1 - Execute");
  ASSERT(LK_FILE_GETWord(fd)==(Word)0x100,"M1 - Execute - L0");
  i+=INSTR_instrSize(execute)*sizeof(Word);
  while(i<M1CodeSize)
  {
    ASSERT(fail==LK_FILE_GET1(fd),"M1 - Instr");
    i+=INSTR_instrSize(fail)*sizeof(Word);
  }
}
