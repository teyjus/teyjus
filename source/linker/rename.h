#ifndef _RENAME_H_
#define _RENAME_H_
#include "datatypes.h"

extern void LK_RENAME_LoadConstRNTable(int fd ,struct Module_st* CMData);
extern void LK_RENAME_LoadKindRNTable(int fd ,struct Module_st* CMData);

extern ConstInd LK_RENAME_RenameConst(char* name);
extern KindInd LK_RENAME_RenameKind(char* name);

#endif //_RENAME_H_
