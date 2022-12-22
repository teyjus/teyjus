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
#include "stringspace.h"
#include "module.h"
#include "vector.h"
#include "file.h"
#include "VectorRW.h"
#include "linker_message.h"
#include "../include/standardlib.h"

typedef struct{
  char* str;
}String_t;

struct Vector Strings;

void LK_STRINGS_Init()
{
  LK_VECTOR_Init(&Strings,10,sizeof(String_t));
}

void LoadString(int fd, struct Module_st* CMData, void* entry)
{
  String_t* StrP=(String_t*)entry;
  char* tmp=LK_FILE_GetString(fd);
  StrP->str=tmp;
}

void LK_STRINGS_Load(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&Strings,CMData,&(CMData->StringsAdj),LoadString);
}

void WriteString(int fd, void* entry)
{
  String_t* StrP=(String_t*)entry;
  LK_FILE_PutString(fd,StrP->str);
  free(StrP->str);
}

void LK_STRINGS_Write(int fd)
{
  debug("Writing String Tables at %lx\n",lseek(fd,0,SEEK_CUR));
  LK_VECTOR_Write(fd,&Strings,WriteString);
  LK_VECTOR_Free(&Strings);
}
