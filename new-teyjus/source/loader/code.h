#ifndef _CODE_H_
#define _CODE_H_

#include "../simulator/mctypes.h"
#include "../system/memory.h"

//!Load the bytecode of the file.
extern int LD_CODE_LoadCode(MEM_GmtEnt* ent);

//!Read a relative code address, return the absolute address.  Relies on code size.
extern WordPtr LD_CODE_GetCodeInd();

//!Read in code size from file and record in MEM_GmtEnt
extern void LD_CODE_LoadCodeSize(MEM_GmtEnt* ent);

#endif //_CODE_H_
