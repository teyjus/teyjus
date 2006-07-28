#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "datatypes.h"

extern void InitTImportTabs();
extern ImportTabInd NewImportTab();
extern void RestoreImportTab();
extern void TopImportTab();
extern void AccImportTab();
extern void ImpImportTab();
extern void WriteImportTabs();
extern void WriteAddCodeTable();

extern void PushCall(ConstInd index,CodeInd addr,int exec_flag);
extern void MarkDynamic(ImportTabInd tab, ConstInd index);


#endif //_IMPORTTAB_H_
