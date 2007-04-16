#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "datatypes.h"
#include "module.h"

extern void InitTImportTabs();
extern ImportTabInd NewImportTab();
extern void RestoreImportTab();
extern void TopImportTab(int fd, struct Module_st* CMData);
extern void AccImportTab(int fd, struct Module_st* CMData);
extern void ImpImportTab(int fd, struct Module_st* CMData);
extern void WriteImportTabs(int fd);
extern void WriteAddCodeTable(int fd);

extern void PushCall(ConstInd index,CodeInd addr,int exec_flag);
extern void MarkDynamic(ImportTabInd tab, ConstInd index);


#endif //_IMPORTTAB_H_
