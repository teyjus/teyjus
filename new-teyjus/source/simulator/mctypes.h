/* Note that this file may need to be moved to ../system/ */

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

typedef unsigned char  BYTE;                       /* 8 bits      */
typedef unsigned char  BOOLEAN;                    /* 8 bits: 0/1 */
typedef unsigned char  FLAG;                       /* 8 bits: ON/OFF */


#define TRUE           1  
#define FALSE          0


typedef void           *WORD;                       

#define WORDSIZE       sizeof(WORD)               /* 4: 32-bits machine */   
                                                  /* 8  64-bits machine */

typedef WORD           MEM;
typedef MEM            *MEM_PTR;

#endif //MCTYPES_H
