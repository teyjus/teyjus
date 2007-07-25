#ifndef _LOADER_H_
#define _LOADER_H_

#include "../system/memory.h"
#include "../system/error.h"
#include "../simulator/mctypes.h"

typedef enum LD_Error{
  LD_LoadError = LOADER_FIRST_ERR_INDEX,
  LD_FILE_OpenError,
  LD_FILE_CloseError,
  LD_FILE_LinkFailure,
  LD_FILE_ReadError
} LD_Error;

extern int LD_LOADER_Load(char* modname, int index);
/* Asking space of given number of WORDS from the system memory */
extern WordPtr LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);
/* Asking space of given number of BYTES from the system memory */
extern BytePtr LD_LOADER_ExtendModSpaceInByte(MEM_GmtEnt* ent, int size);
//extern MEM_GmtEnt* LD_LOADER_GetNewGMTEnt();
//extern void LD_LOADER_DropGMTEnt(MEM_GmtEnt* ent);


#endif //_LOADER_H_
