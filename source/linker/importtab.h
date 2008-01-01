#ifndef _IMPORTTAB_H_
#define _IMPORTTAB_H_

#include "datatypes.h"
#include "module.h"
#include "CallResolution.h"

extern void InitTImportTabs();
extern ImportTabInd NewImportTab();
extern PredInfoTab* GetPredInfoTab();
extern void RestoreImportTab();
extern void TopImportTab(int fd, struct Module_st* CMData);
extern void AccImportTab(int fd, struct Module_st* CMData);
extern void ImpImportTab(int fd, struct Module_st* CMData);
extern void WriteImportTabs(int fd);
extern void WriteAddCodeTable(int fd);
extern void LK_IMPORT_AssignSegmentId(struct Module_st* CMData);

extern Boolean shouldTidySwitchOnReg(ConstInd ind);

#endif //_IMPORTTAB_H_
