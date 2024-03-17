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

/****************************************************************************
 *                                                                          *
 * File compexp.h{c}                                                        * 
 *                                                                          *
 * Evaluating comparison:                                                   *
 * term1 <comp op> term2                                                    *
 *                                                                          *
 * The pointers to term1, term2 are in REG(1) and REG(2).                   *
 *                                                                          *
 * Approach:                                                                *
 * We don't want to repeat work already done in BIEVAL_eval(), and we don't *
 * want to maintain two versions of evaluation.  The implementation of "is" *
 * provides three functions (evalInt, evalFloat, evalString)                *
 * which are used in the current routine.                                   *
 ****************************************************************************/

#ifndef COMPEXP_H
#define COMPEXP_H

void BICOMP_comp();

#endif  //COMPEXP_H
