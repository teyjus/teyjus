#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "module.h"
#include "kind.h"
#include "tyskel.h"
#include "const.h"
#include "stringspace.h"
#include "implgoal.h"
#include "hashtab.h"
#include "bvrtab.h"
#include "code.h"
#include "importtab.h"
#include "file.h"
#include "rename.h"
#include "../system/error.h"
#include "message.h"

#define BC_VER 2
#define LINKCODE_VER 3

static void LoadAccModule(char* modname);
static void LoadAccModules();
static void LoadImpModule(char* modname);
static void LoadImpModules();

static int NumSegs=0;

void CheckBytecodeVersion(int fd)
{
  int x=(int)LK_FILE_GETWord(fd);
  mutter("Bytecode version is %d.\n",x);
  if(x!=BC_VER)
  {
    perror("Incorrect Bytecode Version");
    EM_THROW(LK_LinkError);
  }
}

void CheckModuleName(int fd, char* modname)
{
  char* name=LK_FILE_GetString(fd);
  if(0!=strcmp(modname,name))
  {
    perror("Module name mismatch");
    EM_THROW(LK_LinkError);
  }
  free(name);
}

struct Module_st* NewModule()
{
  struct Module_st* CMData=calloc(1,sizeof(struct Module_st));
  if(CMData==NULL)
  {
    perror("Memory Allocation Failed");
    EM_THROW(LK_LinkError);
  }
  CMData->Pit=GetPredInfoTab();
  return CMData;
}

void InitAll()
{
  InitTLKinds();
  InitTTySkels();
  InitTLConsts();
  InitTHConsts();
  LK_STRINGS_Init();
  InitTImplGoals();
  InitTHashTabs();
  InitTBvrTabs();
  InitTCode();
  InitTImportTabs();
}

void LoadTopModule(char* modname)
{
  mutter("Loading %s as top level module\n",modname);
  EM_TRY{
  NewImportTab();
  struct Module_st* CMData=NewModule();
  int fd = LK_FILE_OpenInput(modname, LK_FILE_ByteCodeExt);
  CheckBytecodeVersion(fd);
  CheckModuleName(fd,modname);
  
  LoadCodeSize(fd,CMData);
  
  LoadTopGKinds(fd,CMData);
  LoadLKinds(fd,CMData);
  
  LoadTySkels(fd,CMData);
  
  LoadTopGConsts(fd,CMData);
  LoadLConsts(fd,CMData);
  LoadHConsts(fd,CMData);
  
  LK_STRINGS_Load(fd,CMData);
  LoadImplGoals(fd,CMData);
  
  LoadHashTabs(fd,CMData);
  LoadBvrTabs(fd,CMData);
  
  TopImportTab(fd,CMData);
  LoadAccModules(fd,CMData);
  LoadImpModules(fd,CMData);
  
  LoadCode(fd,CMData);
  
  LK_FILE_Close(fd);
  free(CMData);
  RestoreImportTab();
  }EM_CATCH{
    bad("Error while reading top level module %s\n",modname);
    EM_RETHROW();
  }
}

void LoadAccModule(char* modname)
{
  struct Module_st* CMData=NewModule();
  int fd = LK_FILE_OpenInput(modname, LK_FILE_ByteCodeExt);
  CheckBytecodeVersion(fd);
  CheckModuleName(fd,modname);
  
  LoadCodeSize(fd,CMData);
  
  LoadGKinds(fd,CMData);
  LoadLKinds(fd,CMData);
  
  LoadTySkels(fd,CMData);
  
  LoadGConsts(fd,CMData);
  LoadLConsts(fd,CMData);
  LoadHConsts(fd,CMData);
  
  LK_STRINGS_Load(fd,CMData);
  LoadImplGoals(fd,CMData);
  
  LoadHashTabs(fd,CMData);
  LoadBvrTabs(fd,CMData);
  
  AccImportTab(fd,CMData);
  LoadAccModules(fd,CMData);
  LoadImpModules(fd,CMData);
  
  LoadCode(fd,CMData);
  
  LK_FILE_Close(fd);
  free(CMData);
}

void LoadImpModule(char* modname)
{
  struct Module_st* CMData=NewModule();
  int fd = LK_FILE_OpenInput(modname, LK_FILE_ByteCodeExt);
  CheckBytecodeVersion(fd);
  CheckModuleName(fd,modname);
  
  LoadCodeSize(fd,CMData);
  
  LoadGKinds(fd,CMData);
  LoadLKinds(fd,CMData);
  
  LoadTySkels(fd,CMData);
  
  LoadGConsts(fd,CMData);
  LoadLConsts(fd,CMData);
  LoadHConsts(fd,CMData);
  
  LK_STRINGS_Load(fd,CMData);
  LoadImplGoals(fd,CMData);
  
  LoadHashTabs(fd,CMData);
  LoadBvrTabs(fd,CMData);
  
  ImpImportTab(fd,CMData);
  LoadAccModules(fd,CMData);
  LoadImpModules(fd,CMData);
  
  LoadCode(fd,CMData);
  
  LK_FILE_Close(fd);
  free(CMData);
}

void LoadImpModules(int fd, struct Module_st* CMData)
{
  int count=CMData->ImportCount=LK_FILE_GET1(fd);
  int i;
  mutter("Importing %d modules\n",count);
  
  if(!count)
  {
    CMData->SegmentID=-1;
    return;
  }
  LK_IMPORT_AssignSegmentId(CMData);
  CMData->Import=EM_malloc(sizeof(ImportTabInd)*count);
  
  for(i=0;i<count;i++)
  {
    char* name=LK_FILE_GetString(fd);
    LK_RENAME_LoadKindRNTable(fd,CMData);
    LK_RENAME_LoadConstRNTable(fd,CMData);
    CMData->Import[i]=NewImportTab();
    LoadImpModule(name);
    RestoreImportTab();
    free(name);
  }
}

void LoadAccModules(int fd, struct Module_st* CMData)
{
  int count=LK_FILE_GET1(fd);
  mutter("Accumulating %d modules\n",count);
  int i;
  for(i=0;i<count;i++)
  {
    char* name=LK_FILE_GetString(fd);
    LK_RENAME_LoadKindRNTable(fd,CMData);
    LK_RENAME_LoadConstRNTable(fd,CMData);
    LoadAccModule(name);
    free(name);
  }
}

void WriteAll(char* modname)
{
  int fd = LK_FILE_OpenOutput(modname,LK_FILE_LinkCodeExt);
  LK_FILE_PUTWord(fd,(Word)LINKCODE_VER);
  LK_FILE_PutString(fd,modname);
  WriteCodeSize(fd);
  WriteKinds(fd);
  WriteTySkels(fd);
  WriteConsts(fd);
  LK_STRINGS_Write(fd);
  WriteImplGoals(fd);
  WriteHashTabs(fd);
  WriteBvrTabs(fd);
  WriteImportTabs(fd);
  WriteCode(fd);
}

KindInd GetKindInd(int fd, struct Module_st* CMData){
  KindInd tmp;
  tmp.gl_flag=LK_FILE_GET1(fd);
  tmp.index=LK_FILE_GET2(fd);
  switch(tmp.gl_flag)
  {
    case LOCAL:
      LK_ADJUST(tmp.index,CMData->LKindAdj,"Local Kind");
      break;
      
    case GLOBAL:
      if(tmp.index>=CMData->GKindcount)
      {
        bad("Invalid Global Kind %d\n",tmp.index);
        EM_THROW(LK_LinkError);
      }
      tmp=CMData->GKind[tmp.index];
      
    case PERVASIVE:
      break;
      
    default:
      bad("Invalid Kind Type %d\n",tmp.gl_flag);
      EM_THROW(LK_LinkError);
      break;
  }
  return tmp;
}

void PutKindInd(int fd, KindInd x)
{
  LK_FILE_PUT1(fd,x.gl_flag);
  LK_FILE_PUT2(fd,x.index);
}

ConstInd GetConstInd(int fd, struct Module_st* CMData){
  ConstInd tmp;
  debug("%x:",lseek(fd,0,SEEK_CUR));
  tmp.gl_flag=LK_FILE_GET1(fd);
  tmp.index=LK_FILE_GET2(fd);
  debug("ConstInd[%d,%d]->",tmp.gl_flag,tmp.index);
  switch(tmp.gl_flag)
  {
    case LOCAL:
      LK_ADJUST(tmp.index,CMData->LConstAdj,"Local Constant");
      break;
    
    case HIDDEN:
      LK_ADJUST(tmp.index,CMData->HConstAdj,"Hidden Constant");
      break;
      
    case GLOBAL:
      if(tmp.index>=CMData->GConstcount)
      {
        bad("Invalid Global Constant %d\n",tmp.index);
        EM_THROW(LK_LinkError);
      }
      tmp=CMData->GConst[tmp.index];
      break;
      
    case PERVASIVE:
      break;
      
    default:
      bad("Invalid Constant Flag %d\n",tmp.gl_flag);
      EM_THROW(LK_LinkError);
      break;
  }
  debug("[%d,%d]\n",tmp.gl_flag,tmp.index);
  return tmp;
}

void PutConstInd(int fd, ConstInd x)
{
  debug("Writing ConstInd[%d,%d] at %x\n",x.gl_flag,x.index,lseek(fd,0,SEEK_CUR));
  LK_FILE_PUT1(fd,x.gl_flag);
  LK_FILE_PUT2(fd,x.index);
}

TySkelInd GetTySkelInd(int fd, struct Module_st* CMData){
  TySkelInd tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->TySkelAdj,"Type Skeleton");
  return tmp;
}

CodeInd GetCodeInd(int fd, struct Module_st* CMData){
  CodeInd tmp=(CodeInd)LK_FILE_GETWord(fd);
  //printf("CodeInd:%d->",tmp);//DEBUG
  if(tmp>=CMData->CodeSize)
  {
    bad("Invalid Code Address %d\n",(int)tmp);
    EM_THROW(LK_LinkError);
  }
  tmp+=CMData->CodeOffset;
  //printf("%d\n",tmp);//DEBUG
  return tmp;
}

ImportTabInd GetImportTabInd(int fd, struct Module_st* CMData)
{
  TwoBytes x=LK_FILE_GET2(fd);
  if(x>=CMData->ImportCount)
  {
    bad("Invalid Import Table %d\n",x);
    EM_THROW(LK_LinkError);
  }
  return CMData->Import[x];
}

ImplGoalInd GetImplGoalInd(int fd, struct Module_st* CMData){
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->ImplGoalAdj,"Implication Goal");
  return tmp;
}

HashTabInd GetHashTabInd(int fd, struct Module_st* CMData){
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->HashTabAdj,"Hash Table");
  return tmp;
}

BvrTabInd GetBvrTabInd(int fd, struct Module_st* CMData)
{
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->HashTabAdj,"Bound Variable Table");
  return tmp;
}

StringInd GetStringInd(int fd, struct Module_st* CMData)
{
  TwoBytes x=LK_FILE_GET2(fd);
  LK_ADJUST(x,CMData->StringsAdj,"String");
  return x;
}