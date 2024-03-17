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

/****************************************************************************/
/*                                                                          */
/*                                                                          */
/* builtins/evalexp.{c,h} are responsible for evaluating expressions,       */
/* that is, transforming an expression such as (2 + sin 0) to (2).  There   */
/* is an ad-hoc form of polymorphism, supported by the presence of three    */
/* functions, one each for integer, floating-point, and string terms.       */
/*                                                                          */
/****************************************************************************/
#ifndef EVALEXP_H
#define EVALEXP_H

#include "../dataformats.h" //to be modified

void   BIEVAL_eval();

int           BIEVAL_evalInt(DF_TermPtr tmPtr);
float         BIEVAL_evalFloat(DF_TermPtr tmPtr);
DF_StrDataPtr BIEVAL_evalStr(DF_TermPtr tmPtr);


#endif //EVALEXP_H 
