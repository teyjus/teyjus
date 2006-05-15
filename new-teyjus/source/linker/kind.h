#ifndef _KIND_H_
#define _KIND_H_

/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the outside view of GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//GKind Header Data
//Note: Special Top Level Loading
typedef struct{
	KindInd index;
}LGKind_t;

void InitTGKinds();
void LoadGKinds();
void LoadTopGKinds();
void WriteGKinds();

//LKind Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LLKind_t;*/

void InitTLKinds();
void LoadLKinds();
void WriteLKinds();

////////////////////////////////////////////
//Utility functions for Kinds
////////////////////////////////////////////
#define GLOBAL_KIND 1
#define LOCAL_KIND 0

typedef struct{
	INT1 gl_flag;
	INT2 index;
}KindInd;

KindInd GetKindInd();

#endif //_KIND_H_