#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/stringspace.h"

#include "TestModule.h"

void __CUT__stringscheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    struct Module_st m;
    
    TEST_CreateM1StringTable(inpipe[1]);
    LK_STRINGS_Load(inpipe[0],&m);
    
    LK_STRINGS_Write(inpipe[1]);
    TEST_CheckM1StringTable(inpipe[0]);
    
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
