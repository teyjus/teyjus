#ifndef _FILE_H_
#define _FILE_H_

#include <stdio.h>
#include "datatypes.h"

//Hidden fd functions
extern void PushInput(char* modname);
extern void PopInput();
extern int PeekInput();
extern int PeekOutput();
extern void SetOutput(char* modname);
extern void CloseOutput();

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

//Exposed fd functions.
extern char* LK_FILE_LinkCodeExt;
extern char* LK_FILE_ByteCodeExt;

extern int LK_FILE_OpenInput(char* modname, char* extension);
extern int LK_FILE_OpenOutput(char* modname, char* extension);
extern void LK_FILE_xPipe(int fd[2]);
extern void LK_FILE_Close(int fd);

extern Word LK_FILE_GETWord(int fd);
extern INT4 LK_FILE_GET4(int fd);
extern TwoBytes LK_FILE_GET2(int fd);
extern Byte LK_FILE_GET1(int fd);

extern Name* LK_FILE_GetName(int fd, Name* name);
extern char* LK_FILE_GetString(int fd);

extern void LK_FILE_PUT1(int fd, Byte x);
extern void LK_FILE_PUT2(int fd, TwoBytes x);
extern void LK_FILE_PUT4(int fd, INT4 x);
extern void LK_FILE_PUTN(int fd, void* data, int n);
extern void LK_FILE_PUTWord(int fd, Word x);
extern void LK_FILE_PutName(int fd, Name name);
extern void LK_FILE_PutString(int fd, char* str);

//#indef DEBUG
extern void LK_FILE_PushPipeInput();
extern void LK_FILE_PipePUT1(Byte x);
extern void LK_FILE_PipePUT2(TwoBytes x);
extern void LK_FILE_PipePUT4(INT4 x);
extern void LK_FILE_PipePUTWord(Word x);
extern void LK_FILE_PipePUTN(void* data,int n);
extern void LK_FILE_PipePutName(char* str);
extern void LK_FILE_PipeInputClose();

extern void LK_FILE_PipeOutput();
extern void LK_FILE_PipeOutputClose();
extern Word LK_FILE_PipeGETWord();
extern INT4 LK_FILE_PipeGET4();
extern TwoBytes LK_FILE_PipeGET2();
extern Byte LK_FILE_PipeGET1();
extern Name* LK_FILE_PipeGetName(Name* name);

//#endif DEBUG

#endif //_FILE_H_
