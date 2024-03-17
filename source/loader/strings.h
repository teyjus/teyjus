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
#ifndef _STRINGS_H_
#define _STRINGS_H_

#include "../system/memory.h"
#include "../simulator/dataformats.h"

/**
* \brief Load the string table of a module.
* \pre The file position is at the beginning of the module files string table, ent points to the module table entry being loaded.
* \post The file position is moved to just after the string table.
* \throw LD_MallocError
**/
void LD_STRING_LoadStrings(MEM_GmtEnt* ent);

/**
 * \brief Load a string.
 * \pre The file position is at the beginning of a string, ent points to the module table entry being loaded.
 * \post The file position is moved to just after the string.
 * \throw LD_MallocError
 **/
DF_StrDataPtr LD_STRING_LoadString(MEM_GmtEnt* ent);

DF_StrDataPtr LD_STRING_GetStringAddr();

void LD_STRING_Cleanup();

#endif //_STRINGS_H_
