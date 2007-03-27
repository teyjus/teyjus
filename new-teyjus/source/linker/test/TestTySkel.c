#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/module.h"

void __CUT__tyskelcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
  
    struct Module_st m;
  
    TEST_CreateM1GKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],(struct Module_st*)&m);
    
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],(struct Module_st*)&m);
  
    TEST_CreateM1TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],(struct Module_st*)&m);
    
    LK_FILE_PUT2(inpipe[1],0xdceb);
    ASSERT(0xdceb==LK_FILE_GET2(inpipe[0]),"File position check");
  
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckLKindTable(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckTySkelTable(inpipe[0]);
  
    free(m.GKind);
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__ctyskelcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
  
    struct Module_st m;
  
    TEST_CreateM1GKindTable(inpipe[1]);
    LoadTopGKinds(inpipe[0],(struct Module_st*)&m);
    
    TEST_CreateM1LKindTable(inpipe[1]);
    LoadLKinds(inpipe[0],(struct Module_st*)&m);
  
    TEST_CreateM1TySkelTable(inpipe[1]);
    LoadTySkels(inpipe[0],(struct Module_st*)&m);
    
    LK_FILE_PUT2(inpipe[1],0xdceb);
    ASSERT(0xdceb==LK_FILE_GET2(inpipe[0]),"File position check");
  
    WriteGKinds(inpipe[1]);
    TEST_CheckGKindTable(inpipe[0]);
    WriteLKinds(inpipe[1]);
    TEST_CheckLKindTable(inpipe[0]);
    
    WriteTySkels(inpipe[1]);
    TEST_CheckTySkelTable(inpipe[0]);
  
    free(m.GKind);
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
