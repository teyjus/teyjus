#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "../system/memory.h"

/**
* \brief Load the string table of a module.
* \pre The file position is at the beginning of the module files string table, ent points to the module table entry being loaded.
* \post The file position is moved to just after the string table.
* \throw LD_MallocError
**/
void LD_STRING_LoadStrings(MEM_GmtEnt* ent);

/**
 * \brief Load a string.
 * \pre The file position is at the beginning of a string, ent points to the module table entry being loaded.
 * \post The file position is moved to just after the string.
 * \throw LD_MallocError
 **/
DF_StrDataPtr LD_STRING_LoadString(MEM_GmtEnt* ent);

DF_StrDataPtr LD_STRING_GetStringAddr(int ind);

void LD_STRING_Cleanup();

#endif //_STRINGS_H_
