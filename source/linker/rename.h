#ifndef _RENAME_H_
#define _RENAME_H_
#include "datatypes.h"

extern void LK_RENAME_LoadConstRNTable();
extern void LK_RENAME_LoadKindRNTable();

extern ConstInd LK_RENAME_RenameConst(Name name);
extern KindInd LK_RENAME_RenameKind(Name name);

#endif //_RENAME_H_
