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

#include "../simulator/mctypes.h"
#include "../system/memory.h"

//!Load the bytecode of the file.
extern int LD_CODE_LoadCode(MEM_GmtEnt* ent, int query);

//!Read a relative code address, return the absolute address.  Relies on code size.
extern CSpacePtr LD_CODE_GetCodeInd();

//!Read in code size from file and record in MEM_GmtEnt
extern void LD_CODE_LoadCodeSize(MEM_GmtEnt* ent);

#endif //_CODE_H_
