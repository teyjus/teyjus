#include "query_c.h"

#include "../system/memory.h"
#include "../system/error.h"
#include "../system/stream.h"
#include "../simulator/mctypes.h"
#include "../simulator/abstmachine.h"
#include "../simulator/dataformats.h"
#include "../simulator/simulator.h"
#include "../simulator/builtins/builtins.h"
#include "../simulator/builtins/readterm.h"
#include "../simulator/printterm.h"

#include <stdio.h>
//#include "../simulator/print.h"

/***************************************************************************/
/* state variable for keeping track of the current query                   */
/***************************************************************************/
static Boolean QUERY_reQuery; /* TRUE if this query has already
                                 begun executing and FALSE otherwise. */

/***************************************************************************/
/*  public functions                                                       */
/***************************************************************************/
/* register the current heap top and the next cell as the positions of     */
/* the type of query and the query; increase the heap top correspondingly; */
/* Also, set QUERY_reQuery to be false                                     */
void QUERY_setTypeAndTermLocation()
{
    RT_setTypeStart(AM_hreg);
    AM_hreg += DF_TY_ATOMIC_SIZE;
    RT_setTermStart(AM_hreg);
    AM_hreg += DF_TM_ATOMIC_SIZE;

    QUERY_reQuery = FALSE;
}


/* solve query */
int QUERY_solveQuery()
{
    EM_TRY {
        if (QUERY_reQuery) {// cause backtracking by setting simulator to fail
            AM_preg = AM_failCode;
        } else { // set up to solve the query `solve(Query)' 
            AM_preg  = AM_solveCode;
            AM_cpreg = AM_proceedCode;
            DF_copyAtomic(RT_getTermStart(), (MemPtr)(AM_reg(1)));
            //PR_printTerm((DF_TermPtr)(AM_reg(1)));
            
            QUERY_reQuery = TRUE;
        }
        //invoke simulator 
        SIM_simulate();
        
    } EM_CATCH {
        if (EM_CurrentExnType == EM_QUERY) {
            PRINT_resetPrintState();
        }
        return EM_CurrentExnType;
    }
}

/* show answers */
int QUERY_showAnswers()
{ 
    EM_TRY {
        STREAM_printf(STREAM_stdout, "\n");
        STREAM_printf(STREAM_stdout,"The answer substitution:\n");
        
        PRINT_showAnswerSubs();
        
        if (AM_nempLiveList()){
            STREAM_printf(STREAM_stdout,"\n");
            STREAM_printf(STREAM_stdout,
                          "The remaining disagreement pairs list:\n") ;
            PRINT_showDisAgreeList();
        }
        
        PRINT_resetPrintState();  /* reset printer state after display */
        return EM_NO_ERR;
    } EM_CATCH {
        return EM_CurrentExnType;
    }
}

void QUERY_setQueryFreeVariables()
{
    PRINT_setQueryFreeVariables();
}

Boolean QUERY_queryHasVars()
{
    PRINT_queryHasVars();
}