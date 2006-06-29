#ifndef _RENAME_H_
#define _RENAME_H_
#include "datatypes.h"

extern void InitRNTables();
extern void LoadConstRNTable();
extern void LoadKindRNTable();

extern ConstInd RenameConst(Name name);
extern KindInd RenameKind(Name name);

#endif //_RENAME_H_
