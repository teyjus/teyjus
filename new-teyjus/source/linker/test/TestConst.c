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
    
    TEST_CreateM1GConstTable(inpipe[1]);
    LoadTopGConsts(inpipe[0],&m);
    
    TEST_CreateM1LConstTable(inpipe[1]);
    LoadLConsts(inpipe[0],&m);
    
    TEST_CreateM1HConstTable(inpipe[1]);
    LoadHConsts(inpipe[0],&m);
    
    LK_FILE_PUT2(inpipe[1],0xdedc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xdedc,"File Position Check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckLKindTable(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckTySkelTable(inpipe[0]);
    
    LK_FILE_PUT2(inpipe[1],0xbebc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xbebc,"File Position Check");
    
    WriteGConsts(inpipe[1]);
    TEST_CheckM1GConstTable(inpipe[0]);
    
    WriteLConsts(inpipe[1]);
    TEST_CheckM1LConstTable(inpipe[0]);
    
    WriteHConsts(inpipe[1]);
    TEST_CheckM1HConstTable(inpipe[0]);
    
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
