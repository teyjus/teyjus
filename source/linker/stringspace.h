#ifndef _STRINGSPACE_H_
#define _STRINGSPACE_H_

#include "module.h"
extern void LK_STRINGS_Init();
extern void LK_STRINGS_Load(int fd, struct Module_st* CMData);
extern void LK_STRINGS_Write(int fd);

#endif
