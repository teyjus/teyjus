#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "../system/memory.h"

void LD_IMPORTTAB_LoadImportTabs(MEM_GmtEnt* ent);

WordPtr LD_IMPORTTAB_GetImportTabAddr();

#endif //_IMPORTTAB_H_
