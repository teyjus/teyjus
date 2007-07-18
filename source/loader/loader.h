#ifndef _LOADER_H_
#define _LOADER_H_

#include "../system/memory.h"
#include "../system/error.h"

typedef enum LD_Error{
  LD_LoadError = LOADER_FIRST_ERR_INDEX,
  LD_FILE_OpenError,
  LD_FILE_CloseError,
  LD_FILE_LinkFailure,
  LD_FILE_ReadError
} LD_Error;

extern int LD_LOADER_Load(char* modname);
extern void* LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);
extern MEM_GmtEnt* LD_LOADER_GetNewGMTEnt();
extern void LD_LOADER_DropGMTEnt(MEM_GmtEnt* ent);

#define DEBUG(x) printf(x);

#endif //_LOADER_H_
