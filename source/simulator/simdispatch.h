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
/***************************************************************************/
/*                                                                         */
/*  File simdispatch.h. The instruction dispatch table used by the         */
/*  simulator is defined here as an array of function pointers, each of    */
/*  which refers to a function realizing a corresponding instruction.      */
/*  These functions are defined in the file ./siminstr.c.                  */
/***************************************************************************/
#ifndef SIMDISPATCH_H
#define SIMDISPATCH_H

//the function pointer type of instructions
typedef void (* SDP_InstrFunctionPtr)();

//instruction dispatch table
extern SDP_InstrFunctionPtr SDP_dispatchTable[]; 


#endif //SIMDISPATCH_H
