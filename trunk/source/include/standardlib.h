//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
#ifndef STANDARD_LIB_H
#define STANDARD_LIB_H

#ifdef _WIN32
// Win32 or Cygwin with -mno-cygwin flag

#include <fcntl.h>
#include <io.h>

// One of these will provide memcpy
#include <memory.h>
#include <string.h>

#include "search.h"

// Let obstack know that we have memcpy
#define HAVE_STRING_H
#include "obstack.h"

//unistd.h
#define pipe(i) (_pipe(i, 1024, _O_TEXT))

//endian.h replacement
#define __WORDSIZE            32

#define	__LITTLE_ENDIAN	      1234
#define	__BIG_ENDIAN          4321
#define __BYTE_ORDER          __LITTLE_ENDIAN
#define __FLOAT_WORD_ORDER    __BYTE_ORDER

#define LITTLE_ENDIAN       __LITTLE_ENDIAN
#define BIG_ENDIAN          __BIG_ENDIAN
#define BYTE_ORDER          __BYTE_ORDER

//sys/types.h replacement
typedef unsigned __int8 u_int8_t;
typedef unsigned __int16 u_int16_t;
typedef unsigned __int32 u_int32_t;
typedef unsigned __int64 u_int64_t;

//byteswap.h replacement
#include "byteswap.h"

#else
#ifdef _WIN64
//Win64

#include <fcntl.h>
#include <io.h>

// One of these will provide memcpy
#include <memory.h>
#include <string.h>

#include "search.h"

// Let obstack know that we have memcpy
#define HAVE_STRING_H
#include "obstack.h"

//unistd.h
#define pipe(i) (_pipe(i, 1024, _O_TEXT))

//endian.h replacement
#define __WORDSIZE            64

#define	__LITTLE_ENDIAN	      1234
#define	__BIG_ENDIAN          4321
#define __BYTE_ORDER          __LITTLE_ENDIAN
#define __FLOAT_WORD_ORDER    __BYTE_ORDER

#define LITTLE_ENDIAN       __LITTLE_ENDIAN
#define BIG_ENDIAN          __BIG_ENDIAN
#define BYTE_ORDER          __BYTE_ORDER

//sys/types.h replacement
typedef unsigned __int8 u_int8_t;
typedef unsigned __int16 u_int16_t;
typedef unsigned __int32 u_int32_t;
typedef unsigned __int64 u_int64_t;

//byteswap.h replacement
#include "byteswap.h"

#else
// Non-win32

#include "search.h"
#include <sys/types.h>
#include <unistd.h>
#include <endian.h>
#include <bits/wordsize.h>
#include <byteswap.h>
#include <fcntl.h>
#include "obstack.h"

#define O_BINARY 0x0000

#endif
#endif
#endif  //STANDARD_LIB_H
