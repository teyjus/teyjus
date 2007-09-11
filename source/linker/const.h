#ifndef _CONST_H_
#define _CONST_H_

//GConst Header Data
//Note: Special Top Level Loading
//Note: Uses CM->GConst & CM->GConstcount

extern void InitTGConsts();
extern void LoadGConsts(int fd, struct Module_st* CMData);
extern void LoadTopGConsts(int fd, struct Module_st* CMData);

//LConst Header Data
//Note: Uses CM->LConstoffset & CM->LConstcount

extern void InitTLConsts();
extern void LoadLConsts(int fd, struct Module_st* CMData);

//HConst Header Data
//Note: Uses CM->HConstoffset & CM->HConstcount

extern void InitTHConsts();
extern void LoadHConsts(int fd, struct Module_st* CMData);

extern void WriteConsts(int fd);

extern TwoBytes PackConstInd(ConstInd ind);
extern ConstInd UnPackConstInd(TwoBytes ind);

#endif