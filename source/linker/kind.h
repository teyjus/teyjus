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
extern void WriteKinds(int fd);
extern void WriteGKinds(int fd);
extern void WriteLKinds(int fd);

extern Byte CheckKindArity(KindInd i);

extern TwoBytes PackKindInd(KindInd ind);
extern KindInd UnPackKindInd(TwoBytes ind);

#endif //_KIND_H_
