#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "datatypes.h"

void InitTImportTabs();
ImportTabInd NewImportTab();
void RestoreImportTab();
void ExtImportTab();
void GetImportTab();
void WriteImportTabs();

void PushCall(ConstInd index,CodeInd addr,int exec_flag);
void MarkDynamic(ImportTabInd tab, ConstInd index);


#endif //_IMPORTTAB_H_
