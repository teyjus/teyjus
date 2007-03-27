#ifndef _STRINGSPACE_H_
#define _STRINGSPACE_H_

#include "linker/module.h"
////////////////////////////////////////////////////////////////////
//Provides functions used in loading the string space of a module.//
////////////////////////////////////////////////////////////////////

//StringSpace Header Data
//Note: Uses CM->StringSpacecount && CM->StringSpaceoffset

extern void LK_STRINGS_Init();
extern void LK_STRINGS_Load(int fd, struct Module_st* CMData);
extern void LK_STRINGS_Write(int fd);

#endif
