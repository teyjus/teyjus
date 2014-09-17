#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/module.h"
#include "linker/const.h"
#include "linker/bvrtab.h"
#include "TestModule.h"

void __CUT__BvrTabcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    struct Module_st m;
    
    TEST_CreateM1CodeSize(inpipe[1]);
    LoadCodeSize(inpipe[0],&m);
    
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
    
    TEST_CreateM1BvrTabs(inpipe[1]);
    LoadBvrTabs(inpipe[0],&m);
    
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
    
    WriteBvrTabs(inpipe[1]);
    TEST_CheckM1BvrTabs(inpipe[0]);
    
    free(m.GKind);
    free(m.GConst);
    FreeCode();
    
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

