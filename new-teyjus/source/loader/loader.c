#include <stdio.h>
#include <string.h>
#include "file.h"
#include "../system/memory.h"
#include "../simulator/mctypes.h"
#include "loader.h"
#include "kind.h"
#include "tyskel.h"
#include "const.h"

#define LINKCODE_EXT ".bc"
#define BYTECODE_EXT ".lp"

void LD_LOADER_LoadLinkcodeVer();

void LD_LOADER_LinkOpen(char* modname);
int LD_LOADER_LoadModuleName(char* modname);
int LD_LOADER_LoadDependencies(char* modname);
MEM_GmtEnt* LD_LOADER_GetNewGMTEnt();
void* LD_LOADER_ExtendModSpace(MEM_GmtEnt* ent, int size);
int LD_LOADER_SetName(MEM_GmtEnt* ent,char* modname);



//Defines the primary procedure of the loader: the load function

//Loads the module into returns it's index in the global module table
//Returns -1 on failure.
int LD_LOADER_Load(char* modname)
{
  EM_TRY{
    LD_LOADER_LinkOpen(modname);
    LD_LOADER_LoadLinkcodeVer();
    LD_LOADER_LoadModuleName(modname);
    LD_LOADER_LoadDependencies(modname);
  }EM_CATCH{
    return -1;///\todo Throw error instead?
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
    return -1;
  }
    
  return 0;
}

void LD_LOADER_LinkOpen(char* modname)
{
  if(!LD_FILE_Exists(modname,LINKCODE_EXT))
      LD_FILE_Link(modname);
  LD_FILE_Open(modname,LINKCODE_EXT);
}

///\todo Move this
#define LINKCODE_VER 1
void LD_LOADER_LoadLinkcodeVer()
{
  int tmp=(int)LD_FILE_GETWORD();
  printf("Version is %d.\n",tmp);
  if(tmp!=LINKCODE_VER)
    EM_THROW(LD_LoadError);
}

///\note Check Purpose of module name in file
int LD_LOADER_LoadModuleName(char* modname)
{
  if(LD_FILE_GET1()!=strlen(modname)+1)
    return -1;
  
  char c;
  int i=0;
  do
  {
    c=LD_FILE_GET1();
    if(c!=modname[i++])
      return -1;
  }while(c!='\0');
  return 0;
}

///\note Consider removing
int LD_LOADER_LoadDependencies(char* modname)
{
  char namelen=0;
  char name[256];
  TwoBytes count = LD_FILE_GET2();
  int i;
  int linktime=LD_FILE_ModTime(modname,LINKCODE_EXT);
  for(i=0;i<count;i++)
  {
    namelen=LD_FILE_GET1();
    LD_FILE_GetString(name,namelen);
    if(LD_FILE_ModTime(name,BYTECODE_EXT)>linktime)
    {
      free(name);
      LD_FILE_Link(modname);
      LD_FILE_Open(modname,LINKCODE_EXT);
      LD_LOADER_LoadLinkcodeVer();
      LD_LOADER_LoadModuleName(modname);
      count=LD_FILE_GET2();
      i=-1;
    }
  }
  return 1;
}

MEM_GmtEnt* LD_LOADER_GetNewGMTEnt()
{
  int i;
  for(i=0;i<MEM_MAX_MODULES;i++)
  {
    if(MEM_modTable[i].modname==NULL)
    {
      MEM_modTable[i].modSpaceEnd=MEM_modTable[i].modSpaceBeg=MEM_memTop;
            MEM_modTable[i].codeSpaceEnd=MEM_modTable[i].codeSpaceBeg=(CSpacePtr)MEM_memBot;
      return MEM_modTable+i;
    }
  }
  return NULL;
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
  void* tmp=(void*)ent->modSpaceEnd;
  ent->modSpaceEnd+=size;
  return tmp;
}

int LD_LOADER_SetName(MEM_GmtEnt* ent,char* modname)
{
  char* namebuf=(char*)LD_LOADER_ExtendModSpace(ent,strlen(modname)+1);
  strcpy(namebuf,modname);
  ent->modname=namebuf;
  return 0;
}
