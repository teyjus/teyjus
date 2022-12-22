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
#ifndef _KIND_H_
#define _KIND_H_

#include "../simulator/mctypes.h"
#include "../system/memory.h"

//!Load the kind table of a file.  Sets the global kind counter.
extern int LD_KIND_LoadKst(MEM_GmtEnt* ent);

//!Read an index in multi-table form and return it in single table form.  Relies on global kind counter.
extern TwoBytes LD_KIND_GetKindInd();
extern TwoBytes LD_KIND_GetKindIndQuery(int query);

extern void LD_KIND_FreeKst(MEM_GmtEnt* ent);

#endif //_KIND_H_
