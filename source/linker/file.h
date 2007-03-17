#ifndef _FILE_H_
#define _FILE_H_

#include "datatypes.h"

extern void PushInput(char* modname);
extern void PopInput();
extern void SetOutput(char* modname);

extern Word GETWord();
extern INT4 GET4();
extern TwoBytes GET2();
extern Byte GET1();

extern Name* GetName(Name* name);

extern void PUT1(Byte x);
extern void PUT2(TwoBytes x);
extern void PUT4(INT4 x);
extern void PUTN(void* data, int n);
extern void PUTWord(Word x);

extern void PutName(Name name);

//#indef DEBUG
void LK_FILE_PushPipeInput();
void LK_FILE_PipePUT1(Byte x);
void LK_FILE_PipePUT2(TwoBytes x);
void LK_FILE_PipePUT4(INT4 x);
void LK_FILE_PipePUTWord(Word x);
void LK_FILE_PipePUTN(void* data,int n);
void LK_FILE_PipePutName(Name name);
//#endif DEBUG

#endif //_FILE_H_
