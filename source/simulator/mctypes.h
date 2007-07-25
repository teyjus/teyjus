/****************************************************************************/
/*                                                                          */
/*                                                                          */
/*   File  mctypes.h.                                                       */
/*   This file contains the definitions of the low-level                    */
/*   data types that are used in constructing the more complex objects that */
/*   are used in data representation and in instruction formats. This file  */
/*   will likely be included by most others defining the overall system.    */
/*                                                                          */
/****************************************************************************/
#ifndef MCTYPES_H
#define MCTYPES_H

typedef unsigned char  Byte;                       /* 8 bits              */
typedef unsigned short TwoBytes;                   /* 16 bits             */


typedef unsigned char  Boolean;                    /* 8 bits: FALSE/TRUE  */
#define TRUE           1  
#define FALSE          0


typedef void           *Word;      
typedef Word           *WordPtr;

#define WORD_SIZE      sizeof(Word)               /* 4: 32-bits machine  */   
                                                  /* 8  64-bits machine  */

typedef Word           Mem;                       /* generic memory type */
typedef Mem            *MemPtr;                   /* pointer to memory   */
typedef Byte           *CSpacePtr;                /* code space pointer  */
typedef Byte           *BytePtr;                  

#endif //MCTYPES_H
