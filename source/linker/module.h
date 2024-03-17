//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#ifndef _MODULE_H_
#define _MODULE_H_

#include "datatypes.h"

struct Module_st;

#include "CallResolution.h"

typedef struct{
  int count;
  int offset;
}Adjust_t;

#define LK_ADJUST(x,adj,typestr) if(x>=adj.count){\
fprintf(stderr,"Invalid %s[%d of %d]\n",typestr,x,adj.count);\
EM_THROW(LK_LinkError);}x+=adj.offset

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
	
  Adjust_t ImplGoalAdj;
	
  Adjust_t HashTabAdj;
	
  Adjust_t BvrTabAdj;
	
  Adjust_t StringsAdj;

  int CodeOffset;
  int CodeSize;
	
  int ImportCount;
  ImportTabInd* Import;
  int SegmentID;
    
  PredInfoTab* Pit;
};

extern struct Module_st* CM; 		//The module currently being loaded

void  LK_setPath(char* path); 
extern void LoadTopModule(char* filename);
extern void InitAll();
extern void WriteAll(char* modname);
extern struct Module_st* NewModule();


//////////////////////
//Utility Functions///
//////////////////////

extern ConstInd GetConstInd(int fd, struct Module_st* CMData);
extern TySkelInd GetTySkelInd(int fd, struct Module_st* CMData);
extern KindInd GetKindInd(int fd, struct Module_st* CMData);
extern ImplGoalInd GetImplGoalInd(int fd, struct Module_st* CMData);
extern HashTabInd GetHashTabInd(int fd, struct Module_st* CMData);
extern BvrTabInd GetBvrTabInd(int fd, struct Module_st* CMData);
extern StringInd GetStringInd(int fd, struct Module_st* CMData);
extern CodeInd GetCodeInd(int fd, struct Module_st* CMData);
extern ImportTabInd GetImportTabInd(int fd, struct Module_st* CMData);

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
