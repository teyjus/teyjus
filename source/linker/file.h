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
#ifndef _FILE_H_
#define _FILE_H_

#include <stdio.h>
#include "datatypes.h"

//Exposed fd functions.
extern char* LK_FILE_LinkCodeExt;
extern char* LK_FILE_ByteCodeExt;

extern int LK_FILE_OpenInput(char* modname, char* extension);
extern int LK_FILE_OpenOutput(char* modname, char* extension);
extern void LK_FILE_xPipe(int fd[2]);
extern void LK_FILE_Close(int fd);

extern Word LK_FILE_GETWord(int fd);
extern INT4 LK_FILE_GET4(int fd);
extern TwoBytes LK_FILE_GET2(int fd);
extern Byte LK_FILE_GET1(int fd);

//extern Name* LK_FILE_GetName(int fd, Name* name);
extern char* LK_FILE_GetString(int fd);

extern void LK_FILE_PUT1(int fd, Byte x);
extern void LK_FILE_PUT2(int fd, TwoBytes x);
extern void LK_FILE_PUT4(int fd, INT4 x);
extern void LK_FILE_PUTN(int fd, void* data, int n);
extern void LK_FILE_PUTWord(int fd, Word x);
//extern void LK_FILE_PutName(int fd, Name name);
extern void LK_FILE_PutString(int fd, char* str);

#endif //_FILE_H_
