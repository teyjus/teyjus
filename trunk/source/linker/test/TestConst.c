#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/module.h"
#include "linker/const.h"
#include "TestModule.h"

void __CUT__Constcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    struct Module_st m;
    
    TEST_CreateM1GKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],&m);
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],&m);
    
    TEST_CreateM1TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],&m);
    
    TEST_CreateM1ConstTables(inpipe[1]);
    LoadTopGConsts(inpipe[0],&m);
    LoadLConsts(inpipe[0],&m);
    LoadHConsts(inpipe[0],&m);
    
    LK_FILE_PUT2(inpipe[1],0xdedc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xdedc,"File Position Check");
    
    WriteKinds(inpipe[1]);
    TEST_CheckM1KindTables(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckM1TySkelTable(inpipe[0]);
    
    LK_FILE_PUT2(inpipe[1],0xbebc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xbebc,"File Position Check");
    
    WriteConsts(inpipe[1]);
    TEST_CheckM1ConstTables(inpipe[0]);
    
    free(m.GKind);
    free(m.GConst);
    
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__accconstcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
  
    struct Module_st m2;
    TEST_CreateM2KindTables(inpipe[1]);
    LoadTopGKinds(inpipe[0],(struct Module_st*)&m2);
    LoadLKinds(inpipe[0],(struct Module_st*)&m2);
    
    TEST_CreateM2TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],(struct Module_st*)&m2);
    
    TEST_CreateM2ConstTables(inpipe[1]);
    LoadTopGConsts(inpipe[0],&m2);
    LoadLConsts(inpipe[0],&m2);
    LoadHConsts(inpipe[0],&m2);
    
    TEST_CreateM1M2KindRenameTable(inpipe[1]);
    LK_RENAME_LoadKindRNTable(inpipe[0],(struct Module_st*)&m2);
    
    TEST_CreateM1M2ConstRenameTable(inpipe[1]);
    LK_RENAME_LoadConstRNTable(inpipe[0],(struct Module_st*)&m2);
    
    struct Module_st m1;
    
    TEST_CreateM1KindTables(inpipe[1]);
    LoadGKinds(inpipe[0],(struct Module_st*)&m1);
    LoadLKinds(inpipe[0],(struct Module_st*)&m1);
    
    TEST_CreateM1TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],(struct Module_st*)&m1);
    
    TEST_CreateM1ConstTables(inpipe[1]);
    LoadGConsts(inpipe[0],&m1);
    LoadLConsts(inpipe[0],&m1);
    LoadHConsts(inpipe[0],&m1);
    
    LK_FILE_PUT2(inpipe[1],0xdedc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xdedc,"File Position Check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckM2GKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckM2LKindTable(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckM2TySkelTable(inpipe[0]);
    
    WriteConsts(inpipe[1]);
    TEST_CheckM2ConstTables(inpipe[0]);
    
    free(m1.GKind);
    free(m2.GKind);
    free(m1.GConst);
    free(m2.GConst);
    
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__CCConstcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    struct Module_st m;
    
    TEST_CreateM1GKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],&m);
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],&m);
    
    TEST_CreateM1TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],&m);
    
    TEST_CreateM1ConstTables(inpipe[1]);
    LoadTopGConsts(inpipe[0],&m);
    LoadLConsts(inpipe[0],&m);
    LoadHConsts(inpipe[0],&m);
    
    LK_FILE_PUT2(inpipe[1],0xdedc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xdedc,"File Position Check");
    
    WriteKinds(inpipe[1]);
    TEST_CheckM1KindTables(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckM1TySkelTable(inpipe[0]);
    
    LK_FILE_PUT2(inpipe[1],0xbebc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xbebc,"File Position Check");
    
    WriteConsts(inpipe[1]);
    TEST_CheckM1ConstTables(inpipe[0]);
    
    free(m.GKind);
    free(m.GConst);
    
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}


