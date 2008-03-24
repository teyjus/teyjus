//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
#include "const.h"
#include "strings.h"
#include "../tables/pervinit.h"
#include "../tables/pervasives.h"
#include "ld_message.h"
//PERV_CONST_NUM
//#define PERV_CONST_NUM 10

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

TwoBytes LD_CONST_numGConsts;
TwoBytes LD_CONST_numLConsts;

int LD_CONST_LoadCst(MEM_GmtEnt* ent)
{
  int i;
  MEM_CstEnt* cst;
  TwoBytes num_glob;
  TwoBytes num_loc;
  TwoBytes num_hid;

  TwoBytes cstsize=LD_FILE_GET2();
  LD_detail("Loading %d consts\n",cstsize);
  
  /* MEM_CstEnt* cst=(MEM_CstEnt*)LD_LOADER_ExtendModSpace(ent,(cstsize+PERV_CONST_NUM)*sizeof(MEM_CstEnt)); -- XQ*/
  cst=(MEM_CstEnt*)LD_LOADER_ExtendModSpace(ent,
                                                        (cstsize+PERV_CONST_NUM) * 
                                                        MEM_CST_ENTRY_SIZE);
  ent->cstBase=(MEM_CstPtr)cst;
  //Copy Pervasive constants.
  PERVINIT_copyConstDataTab(cst);
  cst+=PERV_CONST_NUM;
  
  //Get the number of global constants
  num_glob=LD_CONST_numGConsts=LD_FILE_GET2();
  if(num_glob>cstsize)
      return -1;
  
  //Load the globals
  for(i=0;i<num_glob;i++)
  {
    cst[i].fixity=LD_FILE_GET1();
    cst[i].precedence=LD_FILE_GET1();
    cst[i].typeEnvSize=LD_FILE_GET1();
    cst[i].neededness=0;
    cst[i].univCount=0;
    cst[i].name=LD_STRING_LoadString(ent);
    cst[i].tskTabIndex=LD_FILE_GET2();
  }
  cst+=num_glob;
  
  //Get the number of local constants
  num_loc=LD_CONST_numLConsts=LD_FILE_GET2();
  if(num_glob+num_loc>cstsize)
      return -1;
  
  //Load the locals
  for(i=0;i<num_loc;i++)
  {
    cst[i].fixity=LD_FILE_GET1();
    cst[i].precedence=LD_FILE_GET1();
    cst[i].typeEnvSize=LD_FILE_GET1();
    cst[i].neededness=0;
    cst[i].univCount=0;
    cst[i].name=NULL;
    cst[i].tskTabIndex=LD_FILE_GET2();
  }
  cst+=num_loc;
  
  //Get the number of hidden constants
  num_hid=LD_FILE_GET2();
  if(num_glob+num_loc+num_hid!=cstsize)
      return -1;
  
  //Load the hidden constants
  for(i=0;i<num_hid;i++)
  {
    cst[i].fixity=0;
    cst[i].precedence=0;
    cst[i].typeEnvSize=0;
    cst[i].neededness=0;
    cst[i].univCount=0;
    cst[i].name=NULL;
    cst[i].tskTabIndex=LD_FILE_GET2();
  }
  return 0;
}

TwoBytes LD_CONST_GetConstInd()
{
  Byte gl=LD_FILE_GET1();
  TwoBytes ind=LD_FILE_GET2();
  switch(gl)
  {
    case HIDDEN:
      ind+=LD_CONST_numLConsts;
    case LOCAL:
      ind+=LD_CONST_numGConsts;
    case GLOBAL:
      ind+=PERV_CONST_NUM;
    case PERVASIVE:
      return ind;
      break;
    default:
      LD_error("Invalid Const type %d\n",gl);
      EM_THROW(LD_LoadError);
      break;
  }
}
