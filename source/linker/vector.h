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
#ifndef _VECTOR_H_
#define _VECTOR_H_
#include <stddef.h>
#include "../include/standardlib.h"
#include "datatypes.h"
/////////////////////////////////////////////////////////////////
//Defines structures and methods for treating extendable arrays//
/////////////////////////////////////////////////////////////////

/*
struct Vector{
  struct obstack obs;
  size_t objSize;
};*/

struct Vector{
  void* data;
  Word usesize;
  Word maxsize;
  size_t objSize;
};

/**
\brief Initialize a vector for use.
\arg vec The vector to initialize.
\arg size The size of the elements the vector holds.
\arg max ignored
**/
extern void LK_VECTOR_Init(struct Vector* vec, Word max, Word size);

/**
\brief Get the number of elements contained by a vector.
**/
extern Word LK_VECTOR_Size(struct Vector* vec);

/**
\brief Increase the size of a vector
\arg vec The vector
\arg count The number of elements to add to the vector.
**/
extern Word LK_VECTOR_Grow(struct Vector* vec, Word count);

/**
\brief Get a pointer to an element of a vector.
\arg vec The vector
\arg index The index of the element to retrieve.
\note Elements are assumed to be allocated contiguosly, so LK_VECTOR_GetPtr(v,0)+elSize*10 = LK_VECTOR_GetPtr(v,10)
\note Calling LK_VECTOR_Grow may invalidate the pointer returned.
**/
extern void* LK_VECTOR_GetPtr(struct Vector* vec, Word index);

/**
\brief Free the memory used by a vector.
\arg vec The vector to free.
**/
extern void LK_VECTOR_Free(struct Vector* vec);

#endif //_VECTOR_H_
