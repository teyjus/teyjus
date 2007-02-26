#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "const.h"

#include "../tables/pervasives.h"
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
	
	TwoBytes cstsize=LD_FILE_GET2();
	MEM_CstEnt* cst=(MEM_CstEnt*)LD_LOADER_ExtendModSpace(ent,(cstsize+PERV_CONST_NUM)*sizeof(MEM_CstEnt));
	
	//Copy Pervasive constants.
    PERV_copyConstDataTab((PERV_ConstData*)cst);
	cst+=PERV_CONST_NUM;
	
    //Get the number of global constants
	TwoBytes num_glob=LD_CONST_numGConsts=LD_FILE_GET2();
	if(num_glob>cstsize)
		return -1;
	
    //Load the globals
	int str_length;
	char* string;
	for(i=0;i<num_glob;i++)
	{
		cst[i].fixity=LD_FILE_GET1();
		cst[i].precedence=LD_FILE_GET1();
		cst[i].typeEnvSize=LD_FILE_GET1();
        cst[i].neededness=LD_FILE_GET1();
		cst[i].univCount=0;
		
		str_length=LD_FILE_GET1();
        string=(char*)LD_LOADER_ExtendModSpace(ent,str_length);///\todo Fix kind string loading to new representation
		LD_FILE_GetString(string,str_length);
		cst[i].name=string;
		
		cst[i].tskTabIndex=LD_FILE_GET2();
	}
    cst+=num_glob;
    
    //Get the number of local constants
	TwoBytes num_loc=LD_CONST_numLConsts=LD_FILE_GET2();
	if(num_glob+num_loc>cstsize)
		return -1;
    
    //Load the locals
	for(i=0;i<num_loc;i++)
	{
		cst[i].fixity=LD_FILE_GET1();
		cst[i].precedence=LD_FILE_GET1();
        cst[i].typeEnvSize=LD_FILE_GET1();
        cst[i].neededness=LD_FILE_GET1();
		cst[i].univCount=0;
		cst[i].name=NULL;
		cst[i].tskTabIndex=LD_FILE_GET2();
	}
    cst+=num_loc;
    
    //Get the number of hidden constants
	TwoBytes num_hid=LD_FILE_GET2();
	if(num_glob+num_loc+num_hid!=cstsize)
		return -1;
	
    //Load the hidden constants
    for(i=0;i<num_hid;i++)
	{
		cst[i].fixity=0;
		cst[i].precedence=0;
        cst[i].typeEnvSize=LD_FILE_GET1();
        cst[i].neededness=LD_FILE_GET1();
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
      EM_THROW(LD_LoadError);
      break;
  }
}
