/****************************************************************************
 *                                                                          *
 *   File simulator.c. This file contains the procedure that emulates the   *
 *   lambda Prolog abstract machine.                                        *
 ****************************************************************************/

#ifndef SIMULATOR_C
#define SIMULATOR_C

#include "simdispatch.h"
#include "abstmachine.h"
#include "trail.h"
#include "../system/error.h" //to be modified
#include "../tables/instructions.h" //to be modified

#include <stdio.h> //temp

void SIM_simulate()
{
  restart_loop:
    EM_TRY {       
        while(1) SDP_dispatchTable[*((INSTR_OpCode *)AM_preg)]();
        /* it's expected that this statement not be reached: the only
           way out of this while loop is by an exception */        
    } EM_CATCH {
        if (EM_CurrentExnType == EM_FAIL) {
            if (AM_botCP()) EM_RETHROW(); //temp 
            else {
                TR_unwindTrail(AM_cpTR());
                AM_initPDL();
                AM_bndFlag = OFF;
                AM_preg = AM_cpNCL();
                goto restart_loop;
            }
        } else  EM_RETHROW();
    }
}

#endif /* SIMULATOR_C */
