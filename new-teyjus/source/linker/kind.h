#ifndef _KIND_H_
#define _KIND_H_

/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the outside view of GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

typedef struct{
	INT1 gl_flag;
	INT2 index;
}KindInd;

//GKind Header Data
//Note: Uses CM->GKind & CM->GKindcount
typedef struct{
	KindInd index;
}LGKind_t;

void InitTGKinds();
void LoadGKinds();
void LoadTopGKinds();
void WriteGKinds();

//LKind Header Data
//Note: Uses CM->LKindoffset & CM->LKindcount


void InitTLKinds();
void LoadLKinds();
void WriteLKinds();

////////////////////////////////////////////
//Utility functions for Kinds
////////////////////////////////////////////
#define GLOBAL_KIND 1
#define LOCAL_KIND 0

KindInd GetKindInd();

#endif //_KIND_H_
