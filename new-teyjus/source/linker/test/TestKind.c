#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/module.h"

void TEST_CreateGKindTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,1); LK_FILE_PutString(fd,"glob0");
  LK_FILE_PUT1(fd,0); LK_FILE_PutString(fd,"glob1");
}

void TEST_CheckGKindTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"NumGlobs");
  ASSERT(LK_FILE_GET1(fd)==1,"Arity");
  ASSERT(!strcmp(LK_FILE_GetString(fd),"glob0"),"g0 namecheck");
  ASSERT(LK_FILE_GET1(fd)==0,"Arity");
  ASSERT(!strcmp(LK_FILE_GetString(fd),"glob1"),"g1 namecheck");
}

void TEST_CreateLKindTable(int fd)
{
  LK_FILE_PUT2(fd,2);
  LK_FILE_PUT1(fd,1);
  LK_FILE_PUT1(fd,0);
}

void TEST_CheckLKindTable(int fd)
{
  ASSERT(LK_FILE_GET2(fd)==2,"NumLocs");
  ASSERT(LK_FILE_GET1(fd)==1,"Arity");
  ASSERT(LK_FILE_GET1(fd)==0,"Arity");
}

void __CUT__Gkindcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    InitTLKinds();
    struct Module_st m;
    
    TEST_CreateGKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],&m);
    
    LK_FILE_PUT2(inpipe[1],0xdceb);
    ASSERT(0xdceb==LK_FILE_GET2(inpipe[0]),"File position check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    
    free(m.GKind);
    LK_KIND_FreeGKinds();
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__Lkindcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    InitTLKinds();
    struct Module_st m;
    
    TEST_CreateGKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],&m);
    TEST_CreateLKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],&m);
    
    LK_FILE_PUT2(inpipe[1],0xdceb);
    ASSERT(0xdceb==LK_FILE_GET2(inpipe[0]),"File position check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckLKindTable(inpipe[0]);
    
    free(m.GKind);
    LK_KIND_FreeGKinds();
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void TEST_CreateKindRenameTable(int fd)
{
  LK_FILE_PUT2(fd,3);
  LK_FILE_PutString(fd,"foo"); LK_FILE_PUT1(fd,GLOBAL); LK_FILE_PUT2(fd,1);
  LK_FILE_PutString(fd,"bar"); LK_FILE_PUT1(fd,LOCAL); LK_FILE_PUT2(fd,1);
  LK_FILE_PutString(fd,"baz"); LK_FILE_PUT1(fd,GLOBAL); LK_FILE_PUT2(fd,0);
}

void __CUT__Renamekindcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    struct Module_st m;
    
    TEST_CreateGKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],&m);
    TEST_CreateLKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],&m);
    
    TEST_CreateKindRenameTable(inpipe[1]);
    LK_RENAME_LoadKindRNTable(inpipe[0],&m);
    

    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
