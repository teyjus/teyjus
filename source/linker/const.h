#ifndef _CONST_H_
#define _CONST_H_

//GConst Header Data
//Note: Special Top Level Loading
//Note: Uses CM->GConst & CM->GConstcount

void InitTGConsts();
void LoadGConsts();
void LoadTopGConsts();
void WriteGConsts();

//LConst Header Data
//Note: Uses CM->LConstoffset & CM->LConstcount

void InitTLConsts();
void LoadLConsts();
void WriteLConsts();

//HConst Header Data
//Note: Uses CM->HConstoffset & CM->HConstcount

void InitTHConsts();
void LoadHConsts();
void WriteHConsts();

#endif
