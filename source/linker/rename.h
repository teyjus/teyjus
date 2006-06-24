#ifndef _RENAME_H_
#define _RENAME_H_
#include "datatypes.h"

void InitRNTables();
void LoadConstRNTable();
void LoadKindRNTable();

ConstInd RenameConst(Name name);
KindInd RenameKind(Name name);

#endif //_RENAME_H_
