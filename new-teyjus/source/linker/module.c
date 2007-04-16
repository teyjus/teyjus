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
#include "deps.h"

#define BC_VER 3
#define LINKCODE_VER 1

static void LoadAccModule(char* modname);
static void LoadAccModules();
static void LoadImpModule(char* modname);
static void LoadImpModules();

static int NumSegs=0;

void CheckBytecodeVersion(int fd)
{
  int x=LK_FILE_GET4(fd);
  printf("Bytecode version %d.\n",x);
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
  struct Module_st* tmp=calloc(1,sizeof(struct Module_st));
  if(tmp==NULL)
  {
    perror("Memory Allocation Failed");
    EM_THROW(LK_LinkError);
  }
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
  struct Module_st* CMData=NewModule();
  int fd = LK_FILE_OpenInput(modname, LK_FILE_ByteCodeExt);
  CheckBytecodeVersion(fd);
  CheckModuleName(fd,modname);
  
  NewImportTab();
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
  
  RestoreImportTab();
  LK_FILE_Close(fd);
  free(CMData);
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
  printf("Importing %d modules\n",count);//DEBUG
  
  if(!count)
  {
    CMData->SegmentID=-1;
    return;
  }
  CMData->Import=EM_malloc(sizeof(ImportTabInd)*count);
  CMData->SegmentID=NumSegs++;
  
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
  printf("Accumulating %d modules\n",count);//DEBUG
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
  WriteDependencies(fd);
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
        printf("Invalid Global Kind %d\n",tmp.index);
        EM_THROW(LK_LinkError);
      }
      tmp=CMData->GKind[tmp.index];
      
    case PERVASIVE:
      break;
      
    default:
      printf("Invalid Kind Type %d\n",tmp.gl_flag);
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
  tmp.gl_flag=LK_FILE_GET1(fd);
  tmp.index=LK_FILE_GET2(fd);
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
        printf("Invalid Global Constant %d\n",tmp.index);
        EM_THROW(LK_LinkError);
      }
      tmp=CMData->GConst[tmp.index];
      break;
      
    case PERVASIVE:
      break;
      
    default:
      printf("Invalid Constant Flag %d\n",tmp.gl_flag);
      EM_THROW(LK_LinkError);
      break;
  }
  //printf("(%d,%d)\n",tmp.gl_flag,tmp.index);//DEBUG
  return tmp;
}

void PutConstInd(int fd, ConstInd x)
{
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
    printf("Invalid Code Address %d\n",(int)tmp);
    exit(0);
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
    printf("Invalid Import Table %d\n",x);
    exit(0);
  }
  return CMData->Import[x];
}

ImplGoalInd GetImplGoalInd(int fd, struct Module_st* CMData){
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->ImplGoalAdj,"Type Skeleton");
  return tmp;
}

HashTabInd GetHashTabInd(int fd, struct Module_st* CMData){
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->HashTabAdj,"Type Skeleton");
  return tmp;
}

BvrTabInd GetBvrTabInd(int fd, struct Module_st* CMData)
{
  TwoBytes tmp=LK_FILE_GET2(fd);
  LK_ADJUST(tmp,CMData->HashTabAdj,"Type Skeleton");
  return tmp;
}

StringInd GetStringInd(int fd, struct Module_st* CMData)
{
  TwoBytes x=LK_FILE_GET2(fd);
  LK_ADJUST(x,CMData->StringsAdj,"String");
  return x;
}
