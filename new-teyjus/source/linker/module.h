#ifndef _MODULE_H_
#define _MODULE_H_

#include "datatypes.h"

typedef struct{
  int count;
  int offset;
}Adjust_t;

#define LK_ADJUST(x,adj,typestr) if(x>=adj.count){printf("Invalid %s(%d)\n",typestr,x);EM_THROW(LK_LinkError);} x+=adj.offset

struct Module_st{
	struct Module_st* parent;

	int GKindcount;
	KindInd* GKind;
	
    Adjust_t LKindAdj;
	
    Adjust_t TySkelAdj;
	
	int GConstcount;
	ConstInd* GConst;
	
    Adjust_t LConstAdj;
    Adjust_t HConstAdj;
	
	int ImplGoalcount;
	int ImplGoaloffset;
	
	int HashTabcount;
	int HashTaboffset;
	
	int BvrTabcount;
	int BvrTaboffset;

    Adjust_t StringsAdj;

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

extern ConstInd GetConstInd(int fd, struct Module_st* CMData);
extern TySkelInd GetTySkelInd(int fd, struct Module_st* CMData);
extern KindInd GetKindInd(int fd, struct Module_st* CMData);
extern ImplGoalInd GetImplGoalInd();
extern HashTabInd GetHashTabInd();
extern BvrTabInd GetBvrTabInd();
extern StringInd GetStringInd(int fd, struct Module_st* CMData);
extern CodeInd GetCodeInd();
extern ImportTabInd GetImportTabInd();

extern void PutConstInd(int fd, ConstInd x);
#define PutTySkelInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)
extern void PutKindInd(int fd, KindInd x);
#define PutImplGoalInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)
#define PutHashTabInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)
#define PutBvrTabInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)
#define PutStringInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)
#define PutCodeInd(fd,x) LK_FILE_PUTWord(fd,(Word)x)
#define PutImportTabInd(fd,x) LK_FILE_PUT2(fd,(TwoBytes)x)

#endif //_MODULE_H_
