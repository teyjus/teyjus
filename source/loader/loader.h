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
#ifndef _LOADER_H_
#define _LOADER_H_

#include "../system/memory.h"
#include "../system/error.h"
#include "../simulator/mctypes.h"

typedef enum LD_Error{
  LD_LoadError = LOADER_FIRST_ERR_INDEX,
  LD_FILE_OpenError,
  LD_FILE_CloseError,
  LD_FILE_LinkFailure,
  LD_FILE_ReadError
} LD_Error;

extern char* LD_LOADER_makePath(char* modname);

extern int LD_LOADER_Load(char* modname, int index);
/* Asking space of given number of WORDS from the system memory */
extern WordPtr LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);
/* Asking space of given number of BYTES from the system memory */
extern BytePtr LD_LOADER_ExtendModSpaceInByte(MEM_GmtEnt* ent, int size);
//extern MEM_GmtEnt* LD_LOADER_GetNewGMTEnt();
//extern void LD_LOADER_DropGMTEnt(MEM_GmtEnt* ent);

void LD_LOADER_setPath(char* path);

#endif //_LOADER_H_
