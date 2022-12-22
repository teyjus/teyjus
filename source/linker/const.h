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
#ifndef _CONST_H_
#define _CONST_H_

//GConst Header Data
//Note: Special Top Level Loading
//Note: Uses CM->GConst & CM->GConstcount

extern void InitTGConsts();
extern void LoadGConsts(int fd, struct Module_st* CMData);
extern void LoadTopGConsts(int fd, struct Module_st* CMData);

//LConst Header Data
//Note: Uses CM->LConstoffset & CM->LConstcount

extern void InitTLConsts();
extern void LoadLConsts(int fd, struct Module_st* CMData);

//HConst Header Data
//Note: Uses CM->HConstoffset & CM->HConstcount

extern void InitTHConsts();
extern void LoadHConsts(int fd, struct Module_st* CMData);

extern void WriteConsts(int fd);

extern TwoBytes PackConstInd(ConstInd ind);
extern ConstInd UnPackConstInd(TwoBytes ind);

#endif
