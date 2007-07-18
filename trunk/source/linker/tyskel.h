#ifndef _TYSKEL_H_
#define _TYSKEL_H_

#include "datatypes.h"
#include "module.h"

extern void InitTTySkels();


extern void LoadTySkels(int fd, struct Module_st* CMData);
extern void WriteTySkels(int fd);
extern int TySkelCmp(TySkelInd a, TySkelInd b);

#endif //_TYSKEL_H_
