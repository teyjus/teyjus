//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
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
