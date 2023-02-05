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
#include "../simulator/mctypes.h"

/***************************************************************************/
/*                        solving queries                                  */
/***************************************************************************/
/* register the current heap top and the next cell as the positions of     */
/* the type of query and the query; increase the heap top correspondingly  */
// NG: No longer in use, since queries are now compiled
/* void QUERY_setTypeAndTermLocation(); */

/* load query */
void QUERY_loadQuery();

/* solve query */
int QUERY_solveQuery();

/* display answers */
int QUERY_showAnswers();
void QUERY_setQueryFreeVariables();
Boolean QUERY_queryHasVars();

void QUERY_setQueryEntryPoint(int loc);
