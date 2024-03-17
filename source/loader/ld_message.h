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
#ifndef _LD_MESSAGE_H_
#define _LD_MESSAGE_H_

#include <stdio.h>

extern int LD_verbosity;

#define LD_debug(...) {if(LD_verbosity>2){fprintf(stderr,__VA_ARGS__);}}

#define LD_detail(...) {if(LD_verbosity>1){fprintf(stderr,__VA_ARGS__);}}

#define LD_mutter(...) {if(LD_verbosity>0){fprintf(stderr,__VA_ARGS__);}}

#define LD_error(...) {fprintf(stderr,__VA_ARGS__);}

#endif
