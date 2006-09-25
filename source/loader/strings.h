#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "../system/memory.h"

void LD_STRING_LoadStrings(MEM_GmtEnt* ent);

WordPtr LD_STRING_GetStringAddr(int ind);

#endif //_STRINGS_H_
