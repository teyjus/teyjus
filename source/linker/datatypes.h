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
#ifndef _DATATYPES_H_
#define _DATATYPES_H_

#include <sys/types.h>
#include "../simulator/mctypes.h"
#include "../system/error.h"

//#define Byte u_int8_t
//#define TwoBytes u_int16_t
#define INT4 u_int32_t
//#define Word int

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

//TySkel symbols
#define ARROW 0
#define KIND 1
#define VARIABLE 2

#define DEBUG(x) printf("%s\n",x)
#define DEBUGNUM(x) printf("-%d-\n",x)

typedef struct{
  int size;
  char* string;
}Name;

#define Clear(name) free(name.string)

typedef struct{
  Byte gl_flag;
  TwoBytes index;
}MarkInd;

typedef MarkInd ConstInd;
typedef MarkInd KindInd;

typedef TwoBytes TySkelInd;
typedef TwoBytes HashTabInd;
typedef TwoBytes StringInd;
typedef TwoBytes BvrTabInd;
typedef TwoBytes ImplGoalInd;
typedef long CodeInd;
typedef int ImportTabInd;

typedef enum {
  LK_LinkError=LINKER_FIRST_ERR_INDEX
}LK_ExnType;

#endif //_DATATYPES_H_
