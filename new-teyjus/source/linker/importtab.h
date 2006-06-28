#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "datatypes.h"

void InitTImportTabs();
void NewImportTab();
void RestoreImportTab();
void ExtImportTab();
void GetImportTab();
void WriteImportTabs();

void PushCall(ConstInd index,CodeInd addr);

#endif //_IMPORTTAB_H_
