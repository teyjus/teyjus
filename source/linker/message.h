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
#ifndef _LK_MESSAGE_H_
#define _LK_MESSAGE_H_

#include <stdio.h>

extern int verbosity;

#define debug(...) if(verbosity>2){fprintf(stderr,__VA_ARGS__);}

#define detail(...) if(verbosity>1){fprintf(stderr,__VA_ARGS__);}

#define mutter(...) if(verbosity>0){fprintf(stderr,__VA_ARGS__);}

#define bad(...) fprintf(stderr,__VA_ARGS__)
#define error bad

#endif
