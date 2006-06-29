#ifndef _CONST_H_
#define _CONST_H_

//GConst Header Data
//Note: Special Top Level Loading
//Note: Uses CM->GConst & CM->GConstcount

extern void InitTGConsts();
extern void LoadGConsts();
extern void LoadTopGConsts();
extern void WriteGConsts();

//LConst Header Data
//Note: Uses CM->LConstoffset & CM->LConstcount

extern void InitTLConsts();
extern void LoadLConsts();
extern void WriteLConsts();

//HConst Header Data
//Note: Uses CM->HConstoffset & CM->HConstcount

extern void InitTHConsts();
extern void LoadHConsts();
extern void WriteHConsts();

#endif
