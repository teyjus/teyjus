//this file should be generated automatically from the instruction format 
//description

/***************************************************************************/
/*                                                                         */
/*  File simdispatch.c. The instruction dispatch table used by the         */
/*  simulator is defined here as an array of function pointers, each of    */
/*  which refers to a function realizing a corresponding instruction.      */
/*  These functions are defined in the file ./siminstr.c.                  */
/***************************************************************************/

#ifndef SIMDISPATCH_C
#define SIMDISPATCH_C

#include "../tables/instructions.h" //to be modified
#include "siminstr.h"
#include "simdispatch.h"

SDP_InstrFunctionPtr SDP_dispatchTable[INSTR_NUM_INSTRS] = {
    SINSTR_get_variable_t
};


#endif //SIMDISPATCH_C
