#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "system/memory.h"
#include "tables/pervasives.h"
#include "loader/file.h"
#include "loader/strings.h"
#include "loader/loader.h"
#include "loader/kind.h"

void __CUT__kindcheck()
{
  EM_TRY{
    LD_FILE_OpenPipe();
    
    MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
    LD_FILE_PipePUT2(3);//Total kinds
    LD_FILE_PipePUT2(2);//Global Kinds
    LD_FILE_PipePUT1(0);
    LD_FILE_PipePutString("g1");
    LD_FILE_PipePUT1(1);
    LD_FILE_PipePutString("g2");
    LD_FILE_PipePUT2(1);//Local Kinds
    LD_FILE_PipePUT1(0);
    
    LD_KIND_LoadKst(gmtEnt);
    
    ASSERT(gmtEnt->kstBase[PERV_KIND_NUM].arity==0,"kind:g1 arity");
    ASSERT(gmtEnt->kstBase[PERV_KIND_NUM+1].arity==1,"kind:g2 arity");
    ASSERT(gmtEnt->kstBase[PERV_KIND_NUM+2].arity==0,"kind:l1 arity");
    ASSERT(!strcmp(MCSTR_toCString((MCSTR_Str)(gmtEnt->kstBase[PERV_KIND_NUM].name+DF_STRDATA_HEAD_SIZE)),"g1"),"kind:g1 name");
    ASSERT(!strcmp(MCSTR_toCString((MCSTR_Str)(gmtEnt->kstBase[PERV_KIND_NUM+1].name+DF_STRDATA_HEAD_SIZE)),"g2"),"kind:g2 name");
    
    LD_FILE_PipePUT1(0);
    LD_FILE_PipePUT2(0);
    ASSERT(LD_KIND_GetKindInd()==(PERV_KIND_NUM),"Global zero resolution");
    
    LD_FILE_PipePUT1(1);
    LD_FILE_PipePUT2(0);
    ASSERT(LD_KIND_GetKindInd()==(PERV_KIND_NUM+2),"Local zero resolution");
    
    LD_LOADER_DropGMTEnt(gmtEnt);
    LD_FILE_ClosePipe();
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
}
