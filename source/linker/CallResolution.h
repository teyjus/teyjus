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
#ifndef _CALLRESOLUTION_H_
#define _CALLRESOLUTION_H_

#include "datatypes.h"
#include "vector.h"
#include "hashtab.h"

typedef struct Vector PredInfoTab;

/**
\brief Initialize a Predicate information table for use.
\arg Pit a pointer to the table to initialize
**/
extern void InitInfoTab(PredInfoTab* Pit);

/**
\brief Report an occurance of a call or execute instruction.
\arg Pit The predicate info table to report to.
\arg index The constant index of the predicate being called.
\arg addr The address of the call instruction.
\arg exec_flag A flag indicating whether the instruction is execute or call.
**/
extern void PushCall(PredInfoTab* Pit, ConstInd index,CodeInd addr,int exec_flag);

/**
\brief That a predicate may be extended at runtime.
\arg Pit The predicate info table to report to.
\arg index The constant index of the predicate which may be extended.
**/
extern void MarkDynamic(PredInfoTab* Pit, ConstInd index);

/**
\brief Resolve all of the predicate calls reported to a predicate info table.
\arg Pit The predicate info table.
\arg PredSearchTab The search table to use to get code addresses.
**/
extern void ResolvePredCalls(PredInfoTab* Pit, HashTab_t* PredSearchTab);

#endif
