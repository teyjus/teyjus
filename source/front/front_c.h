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
/*                       system initialization                             */
/***************************************************************************/
int FRONT_systemInit(int memSize);

/***************************************************************************/
/*                       link and load                                     */
/***************************************************************************/
int FRONT_link(char* modName);
int FRONT_load(char* modName, int index);
int FRONT_setPath(char* path);

/***************************************************************************/
/*                simulator memory partition                               */
/***************************************************************************/
int FRONT_simulatorInit();
int FRONT_simulatorReInit(Boolean inDoInitializeImports);

/***************************************************************************/
/*                   install and open module                               */
/***************************************************************************/
int FRONT_topModuleInstall();
int FRONT_moduleInstall(int ind);
int FRONT_initModuleContext();


void FRONT_cleanModule();
