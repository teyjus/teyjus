#include "file.h"
#include "../system/memory.h"
#include "../tables/pervasives.h"
#include "../tables/pervinit.h"
#include "loader.h"
#include "kind.h"
#include "ld_message.h"
#include <stdio.h>

#define GLOBAL 0
#define LOCAL 1
#define PERVASIVE 3

TwoBytes LD_KIND_numGKinds=-1;

int LD_KIND_LoadKst(MEM_GmtEnt* ent)
{
  int i;
  char str_length;
  char* string=NULL;
  
  //Allocate space for the kind table.
  TwoBytes kstsize=LD_FILE_GET2();
  LD_detail("Loading %d kinds\n",kstsize);
  MEM_KstEnt* kst=
      (MEM_KstEnt*)LD_LOADER_ExtendModSpace(ent,(kstsize+PERV_KIND_NUM)*sizeof(MEM_KstEnt));
  ent->kstBase=(MEM_KstPtr)kst;
  
  //Copy the pervasive kinds
  PERVINIT_copyKindDataTab(kst);
  kst+=PERV_KIND_NUM;
          
  //Get the number of global kinds
  TwoBytes num_glob=LD_KIND_numGKinds=LD_FILE_GET2();
  //printf("GKind table size=%d\n",num_glob);
  if(num_glob>kstsize)
    return -1;
  
  //Load the global kinds
  for(i=0;i<num_glob;i++)
  {
    kst[i].arity=LD_FILE_GET1();
    kst[i].name=LD_STRING_LoadString(ent);
  }
  
  //Load the local kinds
  TwoBytes num_loc=LD_FILE_GET2();
  //printf("LKind table size=%d\n",num_loc);
  if(num_glob+num_loc!=kstsize)
    return -1;
  kst+=num_glob;
  for(i=0;i<num_loc;i++)
  {
    kst[i].arity=LD_FILE_GET1();
    kst[i].name=NULL;
  }
  
  return 0;
}

TwoBytes LD_KIND_GetKindInd()
{
  Byte gl=LD_FILE_GET1();
  TwoBytes ind=LD_FILE_GET2();
  //printf("Read KIndex %d:%d\n",gl,ind);
  switch(gl)
  {
    case LOCAL:
      ind+=LD_KIND_numGKinds;
    case GLOBAL:
      ind+=PERV_KIND_NUM;
    case PERVASIVE:
      return ind;
    default:
      LD_bad("Invalid Kind type %d\n",gl);
      EM_THROW(LD_LoadError);
  }
}