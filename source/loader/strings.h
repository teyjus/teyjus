#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "../system/memory.h"

/**
* \brief Load the string table of a module.
* \pre The file position is at the beginning of the module files string table, ent points to the module table entry being loaded, MEM_MemTop points to an unused block of memory.
* \post The file position is moved to just after the string table, MEM_MemTop points to the end of the block of loaded strings.
* \throw LD_MallocError
**/
void LD_STRING_LoadStrings(MEM_GmtEnt* ent);

WordPtr LD_STRING_GetStringAddr(int ind);

void LD_STRING_Cleanup();

#endif //_STRINGS_H_
