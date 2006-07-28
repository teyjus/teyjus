#ifndef _FILE_H_
#define _FILE_H_

#include "datatypes.h"

extern void PushInput(char* modname);
extern void PopInput();
extern void WriteDependencies();
extern void SetOutput(char* modname);

extern int GETWORD();
extern INT4 GET4();
extern INT2 GET2();
extern INT1 GET1();

extern Name* GetName(Name* name);

extern void PUT1(INT1 x);
extern void PUT2(INT2 x);
extern void PUT4(INT4 x);
extern void PUTN(void* data, int n);
extern void PUTWORD(int x);

extern void PutName(Name name);

#endif //_FILE_H_
