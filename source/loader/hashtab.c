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
#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "searchtab.h"
#include "ld_message.h"

TwoBytes LD_HASHTAB_numHashTabs;
WordPtr* LD_HASHTAB_HashTabs;

void LD_HASHTAB_LoadHashTabs(MEM_GmtEnt* ent, int query)
{
  int i;
  int ignore;///\note We do not check if the size given for a hash table matches the size used in code.
  TwoBytes count=LD_HASHTAB_numHashTabs=LD_FILE_GET2();
  LD_detail("Loading %d hash tables\n",count);
  LD_HASHTAB_HashTabs=(WordPtr*)EM_malloc(count*sizeof(WordPtr));
  
  for(i=0;i<count;i++)
  {   
    LD_HASHTAB_HashTabs[i]=LD_SEARCHTAB_LoadHashTab(ent,&ignore,query);
  }
  return;
}

WordPtr LD_HASHTAB_GetHashTabAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_HASHTAB_numHashTabs)
    EM_THROW(LD_LoadError);
  return LD_HASHTAB_HashTabs[i];
}


void LD_HASHTAB_Cleanup()
{
  if(LD_HASHTAB_HashTabs){
	free(LD_HASHTAB_HashTabs);
    LD_HASHTAB_HashTabs=NULL;
  }
}
