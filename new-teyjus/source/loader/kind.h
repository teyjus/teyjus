#ifndef _KIND_H_
#define _KIND_H_

#include "../simulator/mctypes.h"
#include "../system/memory.h"

extern int LD_KIND_LoadKst(MEM_GmtEnt* ent);

extern TwoBytes LD_KIND_GetKindInd();
extern TwoBytes LD_KIND_ConvKindInd(Byte flag, TwoBytes index);

#endif //_KIND_H_
