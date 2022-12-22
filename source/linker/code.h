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
#ifndef _CODE_H_
#define _CODE_H_

#include "datatypes.h"
#include "module.h"

extern void InitTCode();
extern void LoadCode(int fd, struct Module_st* CMData);
extern void WriteCode(int fd);
extern void WriteCodeSize(int fd);
extern void LoadCodeSize(int fd, struct Module_st* CMData);

extern CodeInd MergeSubSequence(CodeInd a, CodeInd b,Byte n);
extern CodeInd MergeDefs(CodeInd a, CodeInd b);

extern int TidySwitchOnReg(CodeInd* pyounger);

extern void MakeCallName(CodeInd from, int arity, ConstInd to);
extern void MakeCall(CodeInd from, int arity, CodeInd to);

#endif
