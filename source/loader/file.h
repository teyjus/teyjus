#ifndef _FILE_H_
#define _FILE_H_

#include "../simulator/mctypes.h"
#include "../system/error.h"

#define EM_FILE_READERROR EM_HOPU_FAIL

/**
 * \brief Make the file modnameextension the open file.
 * \throw LD_FILE_OpenError
 **/
extern void LD_FILE_Open(char* modname, char* extension);

/**
 * \brief Close the open file.
 * \throw LD_FILE_CloseError
 **/
extern void LD_FILE_Close();

/**
 * \brief Check if the file modnameextension exist.
 * \return 1 if it exists, 0 otherwise.
 **/
extern int LD_FILE_Exists(char* modname, char* extension);

/**
 * \brief Run the linker to create the executable modname.lp
 * \throw LD_FILE_LinkFailure
**/
extern void LD_FILE_Link(char* modname);

/**
 * \brief Get the modification time of the file modnameextension.
 * \throw LD_FILE_OpenError
 * \return The modification time.
**/
extern int  LD_FILE_ModTime(char* modname, char* extension);

//!Get the next word of the open file.
extern Word LD_FILE_GETWORD();

//!Get the next two bytes of the open file.
extern TwoBytes LD_FILE_GET2();

//!Get the next byte of the open file.
extern Byte LD_FILE_GET1();

/**
 * \brief Write the next length bytes of the open file to buffer as a null terminated string.
 * \pre buffer points to a buffer with at least length+1 bytes of space.
**/
extern void LD_FILE_GetString(char* buffer,int length);

//#ifdef DEBUG
extern void LD_FILE_OpenPipe();
extern void LD_FILE_PipePUT1(Byte b);
extern void LD_FILE_PipePUT2(TwoBytes s);
extern void LD_FILE_PipePUTWORD(Word w);
extern void LD_FILE_PipePutString(char* str);
extern void LD_FILE_ClosePipe();
//#endif

#endif //_FILE_H_
