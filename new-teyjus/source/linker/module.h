#ifndef _MODULE_H_
#define _MODULE_H_

#define INT1 char
#define INT2 short
#define INT4 int

#include "kind.h"
#include "const.h"
#include "tyskel.h"
#include "implgoal.h"
#include "hashtab.h"
#include "bvrtab.h"

struct Module_st{
	struct Module_st* parent;

	int GKindcount;
	LGKind_t*	GKind;
	
	int LKindcount;
	int LKindoffset;
	
	int TySkelcount;
	int TySkeloffset;
	
	int GConstcount;
	LGConst_t*	GConst;
	
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

	int Codeoffset;
	int Codesize;

	int FindCodeFun;
	LImportTab_t ImportTab;
};

typedef Module Module_st;

Module* CM; 		//The module currently being loaded

void LoadTopModule(char* filename);
#endif //_MODULE_H_
