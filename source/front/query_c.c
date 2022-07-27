//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
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

#include "../tables/instructions.h"
#include "../simulator/io-datastructures.h"

#include <stdio.h>

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
  // for compiled queries, we only do this because readterm expects
  // term/typeStartLoc to be initialized when calling RT_initLocalTabs
    RT_setTypeStart(AM_hreg);
    AM_hreg += DF_TY_ATOMIC_SIZE;
    RT_setTermStart(AM_hreg);
    AM_hreg += DF_TM_ATOMIC_SIZE;

    QUERY_reQuery = FALSE;
}
void QUERY_setQueryEntryPoint(int startLoc)
{
  // A query is always compiled as
  // 0x0:fail
  // 0x8:try_me_else #0, 0x0
  // 0x18:...
  // But we don't need a try_me_else since there is only one clause for main

  // This is the instruction right after try_me_else
  /* AM_preg += 0x18; */

  // A more robust solution would be startLoc + (INSTR_I1LX_LEN)
  AM_preg += (startLoc + INSTR_I1LX_LEN);

}
int QUERY_solveQuery()
{
  printf("Solving query...\n");
  EM_TRY {
	if (QUERY_reQuery) {// cause backtracking by setting simulator to fail
	  AM_preg = AM_failCode;
	} else {
	  printf("EntryPoint: %x\n", AM_preg);
	  AM_cpreg = AM_proceedCode;

	  printf("NumFreeVars: %d\n", IO_freeVarTabTop);
	  // Initialize register arguments to IO_freeVarTab
	  for(int i = 0; i < IO_freeVarTabTop; i++){
		// I believe the first register is reserved...
		DF_mkRef(AM_reg(i+1),IO_freeVarTab[i].rigdes);
		QUERY_reQuery = TRUE;
	  }
	  /* printf("NumTypeVars: %d\n", IO); */
	  // Initialize type register arguments
	  for(int i = IO_freeVarTabTop; i < IO_freeVarTabTop + IO_freeVarTabTop; i++){
	    DF_mkFreeVarType(AM_hreg);
		DF_mkRefType(AM_reg(i+1), AM_hreg);
		printf("Making typevar[%d]: %x\n", (i+1),AM_hreg);
		AM_hreg += DF_TY_ATOMIC_SIZE;
	  }
	}
	//invoke simulator 
	SIM_simulate();
	
        
  } EM_CATCH {
	if (EM_CurrentExnType == EM_QUERY) {
	  PRINT_resetPrintState();
	}
	return EM_CurrentExnType;
  }
  return EM_NO_ERR;
}

/* solve query */
/* int QUERY_solveQuery() */
/* { */
/*     EM_TRY { */
/*         if (QUERY_reQuery) {// cause backtracking by setting simulator to fail */
/*             AM_preg = AM_failCode; */
/*         } else { // set up to solve the query `solve(Query)'  */
/*             AM_preg  = AM_solveCode; */
/*             AM_cpreg = AM_proceedCode; */
/*             DF_copyAtomic(RT_getTermStart(), (MemPtr)(AM_reg(1))); */
/*             //PR_printTerm((DF_TermPtr)(AM_reg(1))); */
            
/*             QUERY_reQuery = TRUE; */
/*         } */
/*         //invoke simulator  */
/*         SIM_simulate(); */
        
/*     } EM_CATCH { */
/*         if (EM_CurrentExnType == EM_QUERY) { */
/*             PRINT_resetPrintState(); */
/*         } */
/*         return EM_CurrentExnType; */
/*     } */
/*     return EM_NO_ERR; */
/* } */

/* show answers */
int QUERY_showAnswers()
{
  DF_TermPtr ptr;
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
    } EM_CATCH {
        return EM_CurrentExnType;
    }
    return EM_NO_ERR;
}

void QUERY_setQueryFreeVariables()
{
    PRINT_setQueryFreeVariables();
}

Boolean QUERY_queryHasVars()
{
    return PRINT_queryHasVars();
}


void QUERY_loadQuery(char* modName)
{
  LD_LOADQ_LoadCompiledQuery(NULL,0,0,modName);
}
