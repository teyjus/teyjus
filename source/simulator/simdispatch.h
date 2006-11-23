/***************************************************************************/
/*                                                                         */
/*  File simdispatch.h. The instruction dispatch table used by the         */
/*  simulator is defined here as an array of function pointers, each of    */
/*  which refers to a function realizing a corresponding instruction.      */
/*  These functions are defined in the file ./siminstr.c.                  */
/***************************************************************************/
#ifndef SIMDISPATCH_H
#define SIMDISPATCH_H

//the function pointer type of instructions
typedef void (* SDP_InstrFunctionPtr)();

//instruction dispatch table
extern SDP_InstrFunctionPtr SDP_dispatchTable[]; 


#endif //SIMDISPATCH_H
