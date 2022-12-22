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
#include "strings.h"
#include "loader.h"
#include "file.h"
#include "../system/memory.h"
#include "../simulator/mcstring.h"
#include "ld_message.h"

TwoBytes LD_STRING_numStrings;
DF_StrDataPtr* LD_STRING_Strings;

void LD_STRING_LoadStrings(MEM_GmtEnt* ent)
{
  int i;
  TwoBytes count=LD_STRING_numStrings=LD_FILE_GET2();
  LD_detail("Loading %d strings\n",count);
  LD_STRING_Strings=(DF_StrDataPtr*)EM_malloc(count*sizeof(char*));
  
  for(i=0;i<count;i++)
  {
    LD_STRING_Strings[i]= LD_STRING_LoadString(ent);
  }
  return;
}

DF_StrDataPtr LD_STRING_LoadString(MEM_GmtEnt* ent)
{
  DF_StrDataPtr loc;
  int str_length=LD_FILE_GET4();
  char* string=(char*)EM_malloc((str_length+1)*sizeof(char));
  LD_FILE_GetString(string,str_length);
  
  loc=(DF_StrDataPtr)LD_LOADER_ExtendModSpace(ent,
                                              MCSTR_numWords(str_length) + 
                                              DF_STRDATA_HEAD_SIZE);
  
  DF_mkStrDataHead((MemPtr)loc);
  MCSTR_toString((MCSTR_Str)(loc+DF_STRDATA_HEAD_SIZE), string, str_length);
  
  free(string);
  return loc;
}

DF_StrDataPtr LD_STRING_GetStringAddr()
{
  int i =(int) LD_FILE_GET2();
  if(0>i || i>LD_STRING_numStrings)
    EM_THROW(LD_LoadError);
  return LD_STRING_Strings[i];
}

void LD_STRING_Cleanup()
{
  if(LD_STRING_Strings)
  {
    free(LD_STRING_Strings);
    LD_STRING_Strings=NULL;
  }
}
