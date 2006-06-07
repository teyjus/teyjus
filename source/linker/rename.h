#ifndef _RENAME_H_
#define _RENAME_H_

void LoadConstRNTable();
void LoadKindRNTable();

ConstInd RenameConst(Name name);
KindInd RenameKind(Name name);

#endif