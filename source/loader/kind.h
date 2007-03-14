#ifndef _KIND_H_
#define _KIND_H_

#include "../simulator/mctypes.h"
#include "../system/memory.h"

//!Load the kind table of a file.  Sets the global kind counter.
extern int LD_KIND_LoadKst(MEM_GmtEnt* ent);

//!Read an index in multi-table form and return it in single table form.  Relies on global kind counter.
extern TwoBytes LD_KIND_GetKindInd();

#endif //_KIND_H_
