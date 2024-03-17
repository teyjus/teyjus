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
#ifndef STANDARD_LIB_H
#define STANDARD_LIB_H

#if defined(_WIN32) || defined(_WIN64)
// Windows or Cygwin with -mno-cygwin flag

#include <io.h>

// unistd.h replacement
#define pipe(i) (_pipe(i, 1024, _O_TEXT))

// endian.h replacement

#if defined(_WIN32)
# define __WORDSIZE            32
#elif defined(_WIN64)
# define __WORDSIZE            64
#endif

#define	__LITTLE_ENDIAN	      1234
#define	__BIG_ENDIAN          4321
#define __BYTE_ORDER          __LITTLE_ENDIAN
#define __FLOAT_WORD_ORDER    __BYTE_ORDER

#define LITTLE_ENDIAN       __LITTLE_ENDIAN
#define BIG_ENDIAN          __BIG_ENDIAN
#define BYTE_ORDER          __BYTE_ORDER

// sys/types.h replacement
typedef unsigned __int8 u_int8_t;
typedef unsigned __int16 u_int16_t;
typedef unsigned __int32 u_int32_t;
typedef unsigned __int64 u_int64_t;

#include "byteswap.h"

#elif defined(__APPLE__)
// Apple Darwin

#include <unistd.h>
#include <machine/endian.h>
#include <sys/types.h>
#include "byteswap.h"
#include <stdint.h>                       // Defines __WORDSIZE

#elif defined(__linux__)
// Linux
#  include <sys/types.h>
#  include <unistd.h>
#  include <endian.h>
#  include <bits/wordsize.h>
#  include <byteswap.h>

#elif defined(__unix__)
// Other Unix
#  include <sys/param.h>
#  if defined(BSD)
// BSD (OpenBSD, FreeBSD, NetBSD, DragonFly BSD, ...)
#    include <sys/types.h>
#    include <unistd.h>
#    include <sys/endian.h>
// Use the functions from byteorder(3)
#    define bswap_16(x) swap16(x)
#    define bswap_32(x) swap32(x)
#    define bswap_64(x) swap64(x)
// __WORDSIZE is non-standard and should be inferred otherhow
#    if defined(__x86_64__) || defined(__amd64__) || defined(__LP64__)
#      define __WORDSIZE 64
#    else
#      define __WORDSIZE 32
#    endif
#  endif

#else
// Other (...?)

#endif  // End platform check


// General includes

#include <fcntl.h>
#include "search.h"

#include <string.h>        // Provides memcpy
#define HAVE_STRING_H      // Let obstack know that we have memcpy
#include "obstack.h"

// Only Cygwin has or cares about the O_BINARY flag
#ifndef O_BINARY
# define O_BINARY 0x0000
#endif

#endif  //STANDARD_LIB_H
