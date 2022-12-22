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
#ifndef _TREE_H_
#define _TREE_H_

#include "datatypes.h"
/**
\brief Add a node to a tree.
\arg root The address of a pointer to the tree root.  Use a pointer to null for an empty tree.
\arg key The string which the key is organized in the root by.
\arg ind The data to associate with the key.
\throw LK_OutOfMemory
**/
extern void LK_TREE_Add(void** root, char* key, MarkInd ind);

/**
\brief Get the data associated with name key.
\throw LK_LinkError
**/
extern MarkInd LK_TREE_Retrieve(void** root, char* key);

/**
 \brief Free the memory used by the tree at root.
**/
extern void LK_TREE_Empty(void** root);
#endif //_TREE_H_
