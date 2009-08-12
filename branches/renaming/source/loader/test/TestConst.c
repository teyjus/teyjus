#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "system/memory.h"
#include "tables/pervasives.h"
#include "loader/file.h"
#include "loader/strings.h"
#include "loader/loader.h"
#include "loader/const.h"

void __CUT__constcheck()
{
  EM_TRY{
    LD_FILE_OpenPipe();
    
    MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
    
    LD_FILE_PipePUT2(3);//Total Consts
    LD_FILE_PipePUT2(2);//Global Consts
    LD_FILE_PipePUT1(0xfe);
    LD_FILE_PipePUT1(0xfd);
    LD_FILE_PipePUT1(0xfc);
    LD_FILE_PipePutString("G1");
    LD_FILE_PipePUT2(0xfafa);
    LD_FILE_PipePUT1(0xde);
    LD_FILE_PipePUT1(0xdd);
    LD_FILE_PipePUT1(0xdc);
    LD_FILE_PipePutString("G2");
    LD_FILE_PipePUT2(0xdada);
    LD_FILE_PipePUT2(1);//Local Consts
    LD_FILE_PipePUT1(0xce);
    LD_FILE_PipePUT1(0xcd);
    LD_FILE_PipePUT1(0xcc);
    LD_FILE_PipePUT2(0xcaca);
    LD_FILE_PipePUT2(0);//Hidden Consts
    
    LD_CONST_LoadCst(gmtEnt);
    
    ASSERT(!strcmp(MCSTR_toCString((MCSTR_Str)(gmtEnt->cstBase[PERV_CONST_NUM].name+DF_STRDATA_HEAD_SIZE)),"G1"),"kind:g1 name");
    ASSERT(!strcmp(MCSTR_toCString((MCSTR_Str)(gmtEnt->cstBase[PERV_CONST_NUM+1].name+DF_STRDATA_HEAD_SIZE)),"G2"),"kind:g2 name");
    ASSERT(gmtEnt->cstBase[PERV_CONST_NUM].fixity==0xfe,"const:G1 fixity");
    
    LD_FILE_PipePUT1(0);
    LD_FILE_PipePUT2(0);
    ASSERT(LD_CONST_GetConstInd()==(PERV_CONST_NUM),"Global zero resolution");
    LD_FILE_PipePUT1(1);
    LD_FILE_PipePUT2(0);
    ASSERT(LD_CONST_GetConstInd()==(PERV_CONST_NUM+2),"Local zero resolution");
    
    LD_LOADER_DropGMTEnt(gmtEnt);
    LD_FILE_ClosePipe();
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
}
