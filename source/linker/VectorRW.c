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
#include "VectorRW.h"
#include "file.h"
#include "vector.h"
#include "module.h"

void LK_VECTOR_Read(int fd, struct Vector* vec, struct Module_st* CMData, Adjust_t* adj,void (*read_fn)(int fd, struct Module_st* CMData, void* entry))
{
  Word i;
  Word offset;
  Word objSize;
  void* base;
  Word count=adj->count=LK_FILE_GET2(fd);
  if(count==0)
  {
    adj->offset=LK_VECTOR_Size(vec);
    return;
  }
  offset=adj->offset=LK_VECTOR_Grow(vec,count);
  base=LK_VECTOR_GetPtr(vec,offset);
  objSize=vec->objSize;
  for(i=0;i<count;i++)
  {
    read_fn(fd,CMData,((u_int8_t *)base)+i*objSize);
  }
}

void LK_VECTOR_Write(int fd, struct Vector* vec,void (*write_fn)(int fd, void* entry))
{
  Word i;
  Word size=LK_VECTOR_Size(vec);
  Word objSize;
  void* base;
  LK_FILE_PUT2(fd,size);
  if(size==0) return;
  base = LK_VECTOR_GetPtr(vec,0);
  objSize= vec->objSize;
  for(i=0;i<size;i++)
  {
    write_fn(fd,((u_int8_t *)base)+i*objSize);
  }
}
