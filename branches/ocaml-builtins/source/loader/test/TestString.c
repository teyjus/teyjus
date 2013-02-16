#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "system/memory.h"
#include "loader/file.h"
#include "loader/strings.h"
#include "loader/loader.h"

void __CUT__stringscheck()
{
  MEM_memInit(1024*8);
  
  EM_TRY{
    LD_FILE_OpenPipe();
    MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
    
    LD_FILE_PipePUT2(3);
    LD_FILE_PipePutString("foo");
    LD_FILE_PipePutString("bar");
    LD_FILE_PipePutString("foobaz");
    
    LD_STRING_LoadStrings(gmtEnt);
    LD_FILE_PipePUT2(0);
    LD_FILE_PipePUT2(1);
    LD_FILE_PipePUT2(2);
    ASSERT(0==strcmp(MCSTR_toCString((MCSTR_Str)(LD_STRING_GetStringAddr()+DF_STRDATA_HEAD_SIZE)),"foo"),"1st String");
    ASSERT(0==strcmp(MCSTR_toCString((MCSTR_Str)(LD_STRING_GetStringAddr()+DF_STRDATA_HEAD_SIZE)),"bar"),"2nd String");
    ASSERT(0==strcmp(MCSTR_toCString((MCSTR_Str)(LD_STRING_GetStringAddr()+DF_STRDATA_HEAD_SIZE)),"foobaz"),"3rd String");
    LD_STRING_Cleanup();
    
    LD_LOADER_DropGMTEnt(gmtEnt);
    LD_FILE_ClosePipe();
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
}
