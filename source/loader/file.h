#ifndef _FILE_H_
#define _FILE_H_

#include "../simulator/mctypes.h"

extern int LD_FILE_Open(char* modname, char* extension);
extern int LD_FILE_Close();
extern int LD_FILE_Exists(char* modname, char* extension);
extern int LD_FILE_Link(char* modname);
extern int  LD_FILE_ModTime(char* modname, char* extension);

extern Word LD_FILE_GETWORD();
extern FourBytes LD_FILE_GET4();
extern TwoBytes LD_FILE_GET2();
extern Byte LD_FILE_GET1();

extern void LD_FILE_GetString(char* buffer,int length);

#endif //_FILE_H_
