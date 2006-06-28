#ifndef _MODULE_H_
#define _MODULE_H_

#include "datatypes.h"

struct Module_st{
	struct Module_st* parent;

	int GKindcount;
	KindInd* GKind;
	
	int LKindcount;
	int LKindoffset;
	
	int TySkelcount;
	int TySkeloffset;
	
	int GConstcount;
	ConstInd* GConst;
	
	int LConstcount;
	int LConstoffset;
	
	int HConstcount;
	int HConstoffset;
	
	int ImplGoalcount;
	int ImplGoaloffset;
	
	int HashTabcount;
	int HashTaboffset;
	
	int BvrTabcount;
	int BvrTaboffset;

	int StringSpacecount;
	int StringSpaceoffset;

	int CodeOffset;
	int CodeSize;

	int FindCodeFun;
	ImportTabInd ImportTabID;
};

struct Module_st* CM; 		//The module currently being loaded

void LoadTopModule(char* filename);
void InitAll();
void WriteAll();

//////////////////////
//Utility Functions///
//////////////////////

ConstInd GetConstInd();
TySkelInd GetTySkelInd();
KindInd GetKindInd();
KindInd FindKindInd(INT1 gl_flag,INT2 index);
ImplGoalInd GetImplGoalInd();
HashTabInd GetHashTabInd();
BvrTabInd GetBvrTabInd();
StringSpaceInd GetStringSpaceInd();
CodeInd GetCodeInd();

void PutConstInd(ConstInd x);
void PutTySkelInd(TySkelInd x);
void PutKindInd(KindInd x);
void PutImplGoalInd(ImplGoalInd x);
void PutHashTabInd(HashTabInd x);
void PutBvrTabInd(BvrTabInd x);
void PutStringSpaceInd(StringSpaceInd x);
void PutCodeInd(CodeInd x);

#endif //_MODULE_H_
