#ifndef STANDARD_LIB_H
#define STANDARD_LIB_H

#ifndef _WIN32

#include <search.h>
#include <sys/types.h>
#include <unistd.h>
#include <endian.h>
#include <bits/wordsize.h>
#include <byteswap.h>
#include <fcntl.h>
#include "obstack.h"

#else
#include <fcntl.h>
#include <io.h>

#include "search.h"

#include <memory.h>
#define memcpy

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

#endif

#endif  //STANDARD_LIB_H
