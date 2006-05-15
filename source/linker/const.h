#endif _CONST_H_
#define _CONST_H_

//GConst Header Data
//Note: Special Top Level Loading
typedef struct{
	ConstInd index;
}LGConst_t;

void InitTGConsts();
void LoadGConsts();
void LoadTopGConsts();
void WriteGConsts();

//LConst Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LLConst_t;*/

void InitTLConsts();
void LoadLConsts();
void WriteLConsts();

//HConst Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LHConst_t;*/

void InitTHConsts();
void LoadHConsts();
void WriteHConsts();

////////////////////////////////////
//Utility Functions
////////////////////////////////////

typedef struct{
	INT1 gl_flag;
	INT2 index;
}ConstInd;

ConstInd GetConstInd();

#endif
