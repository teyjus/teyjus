#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "system/memory.h"
#include "tables/pervasives.h"
#include "loader/file.h"
#include "loader/strings.h"
#include "loader/loader.h"
#include "loader/bvrtab.h"

void __CUT__bvrtabcheck()
{
  EM_TRY{
    LD_FILE_OpenPipe();
    
    MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
    
    LD_FILE_PipePUTWORD(0x10000);
    LD_CODE_LoadCodeSize(gmtEnt);
    
    LD_FILE_PipePUT2(2);
    LD_FILE_PipePUT2(3);
    LD_FILE_PipePUT1(3); LD_FILE_PipePUTWORD(0xf0f0);
    LD_FILE_PipePUT1(2); LD_FILE_PipePUTWORD(0xd0d0);
    LD_FILE_PipePUT1(1); LD_FILE_PipePUTWORD(0xc0c0);
    LD_FILE_PipePUT2(2);
    LD_FILE_PipePUT1(1); LD_FILE_PipePUTWORD(0xb0b0);
    LD_FILE_PipePUT1(0); LD_FILE_PipePUTWORD(0xa0a0);
    
    LD_BVRTAB_LoadBvrTabs(gmtEnt);
    
    LD_FILE_PipePUT2(0xdbdb);
    ASSERT(LD_FILE_GET2()==0xdbdb,"File position check");
    
    LD_LOADER_DropGMTEnt(gmtEnt);
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
}
