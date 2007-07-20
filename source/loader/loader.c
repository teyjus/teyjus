#include <stdio.h>
#include <string.h>
#include "ld_message.h"
#include "file.h"
#include "../system/memory.h"
#include "../simulator/mctypes.h"
#include "loader.h"
#include "kind.h"
#include "tyskel.h"
#include "const.h"

#define LINKCODE_EXT ".lp"
#define BYTECODE_EXT ".lpo"

void LD_LOADER_LoadLinkcodeVer();

int LD_LOADER_LoadModuleName(char* modname);
MEM_GmtEnt* LD_LOADER_GetNewGMTEnt();
void* LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);
int LD_LOADER_SetName(MEM_GmtEnt* ent,char* modname);

int LD_verbosity = 0;

//Defines the primary procedure of the loader: the load function

//Loads the module into returns it's index in the global module table
//Returns -1 on failure.
int LD_LOADER_Load(char* modname)
{
  EM_TRY{
    LD_FILE_Open(modname,LINKCODE_EXT);
    LD_LOADER_LoadLinkcodeVer();
    LD_LOADER_LoadModuleName(modname);
  }EM_CATCH{
    EM_THROW(LD_LoadError);
  }
  MEM_GmtEnt* gmtEnt=LD_LOADER_GetNewGMTEnt();
  EM_TRY{
    LD_LOADER_SetName(gmtEnt,modname);
    LD_CODE_LoadCodeSize(gmtEnt);
    LD_KIND_LoadKst(gmtEnt);
    LD_TYSKEL_LoadTst(gmtEnt);
    LD_CONST_LoadCst(gmtEnt);
    LD_STRING_LoadStrings(gmtEnt);
    LD_IMPLGOAL_LoadImplGoals(gmtEnt);
    LD_HASHTAB_LoadHashTabs(gmtEnt);
    LD_BVRTAB_LoadBvrTabs(gmtEnt);
    LD_IMPORTTAB_LoadImportTabs(gmtEnt);
    LD_CODE_LoadCode(gmtEnt);
  }EM_CATCH{
    ///\todo Clean up after failed load.
    LD_LOADER_DropGMTEnt(gmtEnt);
    EM_THROW(LD_LoadError);
  }
    
  return 0;
}

#define LINKCODE_VER 3
void LD_LOADER_LoadLinkcodeVer()
{
  int tmp=(int)LD_FILE_GETWORD();
  if(tmp!=LINKCODE_VER){
    LD_bad("Incompatible linkcode version %d.\n",tmp);
    EM_THROW(LD_LoadError);
  }
}

///\note Check Purpose of module name in file
int LD_LOADER_LoadModuleName(char* modname)
{
  char buf[1024];
  int len=LD_FILE_GET1();
  LD_FILE_GetString(buf,len);
  if(0!=strcmp(buf,modname)){
    LD_bad("Unexpected module name %s in %s%s",buf,modname,LINKCODE_EXT);
    EM_THROW(LD_LoadError);
  }
}

MEM_GmtEnt* LD_LOADER_GetNewGMTEnt()
{
  int i;
  MEM_GmtEnt* ent = MEM_findFreeModTableEntry();
  ent->modSpaceEnd=ent->modSpaceBeg=MEM_memTop;
  ent->codeSpaceEnd=ent->codeSpaceBeg=MEM_memBot;
  return ent;
}

void LD_LOADER_DropGMTEnt(MEM_GmtEnt* ent)
{
  ent->modname=NULL;
}

void LD_LOADER_AddGMTEnt(MEM_GmtEnt* ent)
{
  MEM_memTop=ent->modSpaceEnd;
  MEM_memBot=(MemPtr)ent->codeSpaceBeg;
}

void* LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size)
{
  void* tmp = (void*) ent->modSpaceEnd;
  ent->modSpaceEnd+=size;
  if(ent->modSpaceEnd>ent->codeSpaceBeg){
    LD_bad("Out of module space.\n");
    EM_THROW(LD_LoadError);
  }
  return tmp;
}

int LD_LOADER_SetName(MEM_GmtEnt* ent,char* modname)
{
  char* namebuf=(char*)LD_LOADER_ExtendModSpace(ent,strlen(modname)+1);
  strcpy(namebuf,modname);
  ent->modname=namebuf;
  return 0;
}
