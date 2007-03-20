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
	
	int ImportCount;
	ImportTabInd* Import;
	int SegmentID;
};

struct Module_st* CM; 		//The module currently being loaded

extern void LoadTopModule(char* filename);
extern void InitAll();
extern void WriteAll();

//////////////////////
//Utility Functions///
//////////////////////

extern ConstInd GetConstInd();
extern TySkelInd GetTySkelInd();
extern KindInd GetKindInd(int fd, struct Module_st* CMData);
//extern KindInd FindKindInd(Byte gl_flag,TwoBytes index);
extern ImplGoalInd GetImplGoalInd();
extern HashTabInd GetHashTabInd();
extern BvrTabInd GetBvrTabInd();
extern StringSpaceInd GetStringSpaceInd();
extern CodeInd GetCodeInd();
extern ImportTabInd GetImportTabInd();

extern void PutConstInd(ConstInd x);
extern void PutTySkelInd(TySkelInd x);
extern void PutKindInd(KindInd x);
extern void PutImplGoalInd(ImplGoalInd x);
extern void PutHashTabInd(HashTabInd x);
extern void PutBvrTabInd(BvrTabInd x);
extern void PutStringSpaceInd(StringSpaceInd x);
extern void PutCodeInd(CodeInd x);
extern void PutImportTabInd(ImportTabInd x);

#endif //_MODULE_H_
