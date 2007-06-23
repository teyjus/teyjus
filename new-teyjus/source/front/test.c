#include <stdio.h>
#include <stdlib.h>
#include "test.h"
#include "../system/memory.h"
#include "../tables/pervasives.h"
#include "../simulator/mcstring.h"
#include "../simulator/dataformats.h"
#include "../simulator/abstmachine.h"

void test()
{
    int i;
    MEM_KstPtr kst;
    MEM_CstPtr cst;
    

    fprintf(stdout, "test\n");
    
    printf("wordSize: %d\n", (int)sizeof(void*));
    printf("memBeg : %u, memEnd : %u, \nmemTop : %u, memBot : %u\n",
           MEM_memBeg, MEM_memEnd, MEM_memTop, MEM_memBot);

    printf("kind table\n");
    kst = MEM_topModule.kstBase;
    
    for (i = 0; i < PERV_KIND_NUM; i++) {
        printf("name : %s, arity: %d\n",
               MCSTR_toCString(DF_strDataValue(kst[i].name)), kst[i].arity);
    }

    printf("const table\n");
    cst = MEM_topModule.cstBase;
    
    for (i = 0; i < PERV_CONST_NUM; i++) {
        if (cst[i].name)
            printf("name : %s, tyenv: %d, tyskel: %d, univcount: %d, prec: %d, fixity %d\n", MCSTR_toCString(DF_strDataValue(cst[i].name)), cst[i].typeEnvSize,
                   cst[i].tskTabIndex, cst[i].univCount, cst[i].precedence,
                   cst[i].fixity);
    }

    printf("memtop: %u, membot: %u\n", MEM_memTop, MEM_memBot);
    printf("heapbeg: %u, pdlend: %u\n", AM_heapBeg, AM_pdlEnd);

    printf("hreg: %u, breg: %u, *breg: %u\n", AM_hreg, AM_breg, (*AM_breg));
    
}

 
