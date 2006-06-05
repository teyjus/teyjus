#ifndef _CONST_H_
#define _CONST_H_

typedef struct{
	INT1 gl_flag;
	INT2 index;
}ConstInd;

//GConst Header Data
//Note: Special Top Level Loading
//Note: Uses CM->GConst & CM->GConstcount
typedef struct{
	ConstInd index;
}LGConst_t;

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

////////////////////////////////////
//Utility Functions
////////////////////////////////////

ConstInd GetConstInd();

#endif
