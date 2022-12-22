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
#ifndef _HASHTAB_H_
#define _HASHTAB_H_

#include "vector.h"

typedef struct Vector HashTab_t;

#include "datatypes.h"
#include "module.h"

//Initialization, Loading and Writing Functions
//for Hash Tables.
extern void InitTHashTabs();
extern void LoadHashTabs(int fd, struct Module_st* CMData);
extern void WriteHashTabs(int fd);

/////////////////////////////////////////
//Utilities////////////////////////////
/////////////////////////////////////////

extern void LoadHashTab(int fd, struct Module_st* CMData,void* entry);
extern void WriteHashTab(int fd, void* entry);

//Returns the index of the hash table entry with key x
//Returns -1 on failure
//HashTab must be a legitimate hash table.
extern int HashTabSearch(HashTab_t* HashTab, ConstInd x);
extern CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x);

//Makes a=a+b and b=0;  Returns size of combined table.
extern Byte MergeHashTabs(HashTabInd a, HashTabInd b,Byte n);
extern void MergeFindCodeTabs(HashTab_t* a, HashTab_t* b);

#endif //_HASHTAB_H_
