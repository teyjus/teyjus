#ifndef _LOADER_H_
#define _LOADER_H_

#include "../system/memory.h"

extern int LD_LOADER_Load(char* modname);
extern void* LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);

#define DEBUG(x) printf(x);

#endif //_LOADER_H_
