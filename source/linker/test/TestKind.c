#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/module.h"
#include "TestModule.h"

void __CUT__kindcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    InitTLKinds();
    struct Module_st m;
    
    TEST_CreateM1GKindTable(inpipe[1]);
    
    LoadTopGKinds(inpipe[0],(struct Module_st*)&m);
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],(struct Module_st*)&m);
    
    LK_FILE_PUT2(inpipe[1],0xdceb);
    ASSERT(0xdceb==LK_FILE_GET2(inpipe[0]),"File position check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckLKindTable(inpipe[0]);
    
    free(m.GKind);
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__Renamekindcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    InitTLKinds();
    struct Module_st m;
    
    TEST_CreateM1GKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],(struct Module_st*)&m);
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],(struct Module_st*)&m);
    
    TEST_CreateM1M2KindRenameTable(inpipe[1]);
    LK_RENAME_LoadKindRNTable(inpipe[0],(struct Module_st*)&m);
    
    struct Module_st m2;
    TEST_CreateM2KindTable(inpipe[1]);
    LoadGKinds(inpipe[0],(struct Module_st*)&m2);
    LoadLKinds(inpipe[0],(struct Module_st*)&m2);
    
    LK_FILE_PUT2(inpipe[1],0xdedc);
    ASSERT(LK_FILE_GET2(inpipe[0])==0xdedc,"File Position Check");
    
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckRNKindTable(inpipe[0]);
    
    free(m.GKind);
    free(m2.GKind);

    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
