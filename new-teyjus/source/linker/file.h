#ifndef _FILE_H_
#define _FILE_H_

#include "datatypes.h"

extern void PushInput(char* modname);
extern void PopInput();
extern void WriteDependencies();
extern void SetOutput(char* modname);

extern int GETWord();
extern INT4 GET4();
extern TwoBytes GET2();
extern Byte GET1();

extern Name* GetName(Name* name);

extern void PUT1(Byte x);
extern void PUT2(TwoBytes x);
extern void PUT4(INT4 x);
extern void PUTN(void* data, int n);
extern void PUTWord(int x);

extern void PutName(Name name);

#endif //_FILE_H_
