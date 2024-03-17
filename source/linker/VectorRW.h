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
#ifndef _VECTORRW_H_
#define _VECTORRW_H_

#include "vector.h"
#include "module.h"

/**
\brief Extend a vector by reading more entries from file
\arg fd A valid file descriptor.
\arg vec A pointer to the vector to extend.
\arg CMData The module data to pass to the read function.
\arg adj Where to store the adjustment count and offset.
\arg read_fn A function to call to read each individual entry.
Extends a vector by reading entries from a file, using the format 2 byte size followed by that number of entries.
 **/
extern void LK_VECTOR_Read(int fd, struct Vector* vec, struct Module_st* CMData, Adjust_t* adj,
  void (*read_fn)(int fd, struct Module_st* CMData, void* entry));

/**
\brief Write the contents of a vector to a file.
\arg fd A valid file descriptor.
\arg vec A pointer to the vector to write.
\arg write_fn A function to call to write each individual entry.
Writes the vector out in the format 2 byte size followed by that number of entries.
 **/
extern void LK_VECTOR_Write(int fd, struct Vector* vec,void (*write_fn)(int fd, void* entry));

#endif
