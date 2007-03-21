#ifndef _KIND_H_
#define _KIND_H_

#include "datatypes.h"
#include "module.h"
/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the outside view of GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//GKind Header Data
//Note: Uses CM->GKind & CM->GKindcount

//Loads the global kinds of a bytecode file.
extern void LoadGKinds(int fd, struct Module_st* CMData);
//Loads the global kinds of the top-level bytecode file.
extern void LoadTopGKinds(int fd, struct Module_st* CMData);

extern void LoadLKinds(int fd, struct Module_st* CMData);

//LKind Header Data
//Note: Uses CM->LKindoffset & CM->LKindcount

//Initializes the Local Kind Vector
extern void InitTLKinds();
//Loads the local kinds of a bytecode file.
extern void LoadLKinds();

//Writes out the contents of the Global and Local Kind Vectors.
extern void WriteKinds();
extern void WriteGKinds(int fd);
extern void WriteLKinds(int fd);


extern int CheckKindArity(KindInd i);

#endif //_KIND_H_
