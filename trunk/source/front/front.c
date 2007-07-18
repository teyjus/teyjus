#include "../system/memory.h"
#include "../tables/pervinit.h"
#include "../simulator/mctypes.h"
#include "../simulator/abstmachine.h"
#include "../simulator/siminit.h"
#include "../simulator/builtins/builtins.h"

#include <stdio.h>

/***************************************************************************/
/*                       system initialization                             */
/***************************************************************************/
// default heap size (in word)
#define FRONT_DEFAULT_SYS_SIZE      8192 * 1024

// variables recording the sizes of the different system components 
static int FRONT_heapSize;
static int FRONT_stackSize;
static int FRONT_trailSize;
static int FRONT_pdlSize;

// heap size : stack size : trail size : pdl size = 7 : 4 : 4 : 1 
static FRONT_setMemorySizes(int memSize)
{
    FRONT_heapSize  = memSize / 16 * 7;
    FRONT_stackSize = memSize / 16 * 4;
    FRONT_trailSize = memSize / 16 * 4;
    FRONT_pdlSize   = memSize / 16 * 1;
}

void FRONT_systemInit(int inSize) 
{
    int memSize = inSize ? (inSize*1024 / WORD_SIZE) : FRONT_DEFAULT_SYS_SIZE;
    
    FRONT_setMemorySizes(memSize);
    /* initialize system memory */
    MEM_memInit(memSize);
    /* initialize pervasive tables */
    PERVINIT_tableInit();
    /* initialize top module */
    MEM_topModuleInit();
}


/*****************************************************************************/
/*                       link and load                                       */
/*****************************************************************************/


/*****************************************************************************/
/*                simulator memory partition                                 */
/*****************************************************************************/
void FRONT_simulatorInit()
{
    //finalize simulator memory components
    AM_heapBeg  = (MemPtr)MEM_memTop;
    AM_heapEnd  = AM_stackBeg = ((MemPtr)MEM_memBeg) + FRONT_heapSize;
    AM_stackEnd = AM_trailBeg = AM_stackBeg + FRONT_stackSize;
    AM_trailEnd = AM_pdlBeg   = AM_trailBeg + FRONT_trailSize;
    AM_pdlEnd   = AM_pdlBeg + (FRONT_pdlSize - 1);

    //initialize simulator error messages
    SINIT_preInit();

    //initialize simulator heap and registers
    SINIT_simInit();
    
    //initialize built-in error messages
    BI_init();
}


/*****************************************************************************/
/*              install and open a module                                    */
/*****************************************************************************/
//needed?
static MemPtr FRONT_ireg;
static MemPtr FRONT_tosreg;

static void FRONT_saveRegs()
{
    FRONT_ireg = AM_ireg;
    FRONT_tosreg = AM_tosreg;
}

/* record symbol table bases */
static void FRONT_initSymbolTableBases()
{
    AM_kstBase = MEM_currentModule -> kstBase;
    AM_tstBase = MEM_currentModule -> tstBase;
    AM_cstBase = MEM_currentModule -> cstBase;
}


/* install top module   */
void FRONT_topModuleInstall()
{
    //register the top module as the current one being used
    MEM_currentModule = &MEM_topModule;
    //save the registers to be restored later; needed?
    FRONT_saveRegs();
    //register symbol table bases
    FRONT_initSymbolTableBases();
}


static void FRONT_addModuleImportPoint()
{
    int n, m, l;
    MemPtr addtab = (MemPtr)(MEM_currentModule -> addtable);
    MemPtr ip;
    
    n = MEM_impNCSEG(addtab);  // n = # code segs (# bc fields)
    m = MEM_impLTS(addtab);    // m = link tab size
    l = AM_NCLT_ENTRY_SIZE * m;  // l = space for next clause table 
    
    ip = AM_tosreg + (AM_BCKV_ENTRY_SIZE * n) + l;
    AM_tosreg = ip + AM_IMP_FIX_SIZE;
    AM_stackError(AM_tosreg);
    
    n = MEM_impNLC(addtab);        // reuse n as the number of local consts
    if (n > 0) {
        AM_mkImptRecWL(ip, m, MEM_impPST(addtab, m, n), MEM_impPSTS(addtab),
                       MEM_impFC(addtab));

        AM_ucreg++;
        AM_initLocs(n, MEM_impLCT(addtab, m));
        AM_ucreg--;
        
    } else AM_mkImptRecWOL(ip, m, MEM_impPST(addtab, m, n),MEM_impPSTS(addtab),
                           MEM_impFC(addtab));
    if (m > 0) AM_mkImpNCLTab(ip, MEM_impLT(addtab), m);

    AM_ireg = ip;    
}

/* install the ith module from the global module table */
static void FRONT_installModule(int ind)
{
    MEM_GmtEnt *module;
    
    //retrieve the module from the global module table and register it
    //as the current one being used
    module = &(MEM_modTable[ind]);
    MEM_currentModule = module;
    //add the code for this module to the program context
    FRONT_addModuleImportPoint();
    //save the new registers to restore later 
    FRONT_saveRegs();
}

/* initialize module context */
static void FRONT_initModuleContext()
{
    int i;
    MemPtr addtable = (MemPtr)(MEM_currentModule -> addtable);
    /* (re)initialize the backchained vector for the module */
    i = MEM_impNCSEG(addtable);
    if (i > 0) 
        AM_initBCKVector(AM_ireg,AM_BCKV_ENTRY_SIZE * MEM_impLTS(addtable),i);
    
    /* increment univ counter if there are hidden constants */
    i = MEM_impNLC(addtable);
    if (i > 0) AM_ucreg++;
}


/* install the ith module in the global module table and open its context */
void FRONT_moduleInstall(int ind)
{
    FRONT_installModule(ind);
    FRONT_initModuleContext();
    //register symbol table bases
    FRONT_initSymbolTableBases();
}


