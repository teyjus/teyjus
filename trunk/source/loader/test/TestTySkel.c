#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "system/memory.h"
#include "tables/pervasives.h"
#include "loader/file.h"
#include "loader/loader.h"
#include "loader/tyskel.h"

void __CUT__pipetyskelcheck()
{
  EM_TRY{
    LD_FILE_OpenPipe();
    
    MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
    //LD_KIND_LoadKst(gmtEnt);
    
    LD_FILE_PipePUT2(2);
    // list A -> A
    LD_FILE_PipePUT1(0);
      LD_FILE_PipePUT1(1); LD_FILE_PipePUT1(2); LD_FILE_PipePUT2(4);
        LD_FILE_PipePUT1(2); LD_FILE_PipePUT1(1);
      LD_FILE_PipePUT1(2); LD_FILE_PipePUT1(1);
    // A
    LD_FILE_PipePUT1(2); LD_FILE_PipePUT1(1);
    
    LD_TYSKEL_LoadTst(gmtEnt);
    
    LD_FILE_PipePUT2(0xc7d6);
    ASSERT(LD_FILE_GET2()==0xc7d6,"File position check");
    
    LD_LOADER_DropGMTEnt(gmtEnt);
    LD_FILE_ClosePipe();
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
}
