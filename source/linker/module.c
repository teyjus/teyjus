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
static void PushModule(char* modname);
static void PopModule();

static void CheckBytecodeVersion();
static void CheckModuleName(char* modname);

static int NumSegs=0;

void PushModule(char* modname)
{
  struct Module_st* tmp=calloc(1,sizeof(struct Module_st));
  if(tmp==NULL)
  {
    perror("Memory Allocation Failed");
    exit(0);
  }
  tmp->parent=CM;
  CM=tmp;
  PushInput(modname);
  CheckBytecodeVersion();
  CheckModuleName(modname);
}

void PopModule()
{
  PopInput();
  struct Module_st* tmp=CM->parent;
  free(CM);
  CM=tmp;
}

void InitAll()
{
  CM=NULL;
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
  printf("Loading Top Level module %s.\n",modname);//DEBUG
  PushModule(modname);
  
  NewImportTab();
  LoadCodeSize();
  
  LoadTopGKinds(PeekInput(),CM);
  LoadLKinds(PeekInput(),CM);
  
  LoadTySkels(PeekInput(),CM);
  
  LoadTopGConsts(PeekInput(),CM);
  LoadLConsts(PeekInput(),CM);
  LoadHConsts(PeekInput(),CM);
  
  LK_STRINGS_Load(PeekInput(),CM);
  LoadImplGoals();
  
  LoadHashTabs();
  LoadBvrTabs();
  
  TopImportTab();
  LoadAccModules();
  LoadImpModules();
  
  LoadCode();
  
  RestoreImportTab();
  PopModule();
  printf("Finished module %s.\n",modname);//DEBUG
}

void LoadAccModule(char* modname)
{
  printf("Accumulating module %s.\n",modname);//DEBUG
  PushModule(modname);
  
  LoadCodeSize();
  
  LoadGKinds(PeekInput(),CM);
  LoadLKinds(PeekInput(),CM);
  
  LoadTySkels(PeekInput(),CM);
  
  LoadGConsts();
  LoadLConsts(PeekInput(),CM);
  LoadHConsts(PeekInput(),CM);
  
  LK_STRINGS_Load(PeekInput(),CM);
  LoadImplGoals();
  
  LoadHashTabs();
  LoadBvrTabs();
  
  AccImportTab();
  LoadAccModules();
  LoadImpModules();
  
  LoadCode();
  PopModule();
  printf("Finished module %s.\n",modname);//DEBUG
}

void LoadImpModule(char* modname)
{
  printf("Importing module %s.\n",modname);//DEBUG
  PushModule(modname);
  
  LoadCodeSize();
  
  LoadGKinds(PeekInput(),CM);
  LoadLKinds(PeekInput(),CM);
  
  LoadTySkels(PeekInput(),CM);
  
  LoadGConsts();
  LoadLConsts(PeekInput(),CM);
  LoadHConsts(PeekInput(),CM);
  
  LK_STRINGS_Load(PeekInput(),CM);
  LoadImplGoals();
  
  LoadHashTabs();
  LoadBvrTabs();
  
  ImpImportTab();
  LoadAccModules();
  LoadImpModules();
  
  LoadCode();
  
  PopModule();
  printf("Finished module %s.\n",modname);//DEBUG
}

void LoadImpModules()
{
  int count=CM->ImportCount=GET1();
  int i;
  printf("Importing %d modules\n",count);//DEBUG
  
  if(!count)
  {
    CM->SegmentID=-1;
    return;
  }
  CM->Import=EM_malloc(sizeof(ImportTabInd)*count);
  CM->SegmentID=NumSegs++;
  
  for(i=0;i<count;i++)
  {
    Name name;
    GetName(&name);
    LK_RENAME_LoadKindRNTable(PeekInput(),CM);
    LK_RENAME_LoadConstRNTable();
    CM->Import[i]=NewImportTab();
    LoadImpModule(name.string);
    RestoreImportTab();
    Clear(name);
  }
}

void LoadAccModules()
{
  int count=GET1();
  printf("Accumulating %d modules\n",count);//DEBUG
  int i;
  for(i=0;i<count;i++)
  {
    Name name;
    GetName(&name);
    LK_RENAME_LoadKindRNTable(PeekInput(),CM);
    LK_RENAME_LoadConstRNTable();
    LoadAccModule(name.string);
    Clear(name);
  }
}

void CheckBytecodeVersion()
{
  int x=GET4();
  printf("Bytecode version %d.\n",x);
  if(x!=BC_VER)
  {
    perror("Incorrect Bytecode Version");
    exit(0);
  }
}

void CheckModuleName(char* modname)
{
  Name name;
  GetName(&name);
  if(strcmp(modname,name.string)!=0)
  {
    perror("Module name mismatch");
    exit(0);
  }
  Clear(name);
}

void WriteAll(char* modname)
{
  SetOutput(modname);
  PUTWord((Word)LINKCODE_VER);
  LK_FILE_PutString(PeekOutput(),modname);
  WriteDependencies();
  WriteCodeSize();
  WriteKinds(PeekOutput());
  WriteTySkels(PeekOutput());
  WriteConsts(PeekOutput());
  LK_STRINGS_Write(PeekOutput());
  WriteImplGoals();
  WriteHashTabs();
  WriteBvrTabs();
  WriteAddCodeTable();
  PUT1(NumSegs);
  WriteImportTabs();
  WriteCode();
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
        printf("Invalid Global Kind %d\n",index);
        EM_THROW(LK_LinkError);
      }
      tmp=CMData->GKind[tmp.index];
      
    case PERVASIVE:
      break;
      
    default:
      printf("Invalid Kind Type %d\n",index);
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

CodeInd GetCodeInd(){
  CodeInd tmp=(CodeInd)GETWord();
  //printf("CodeInd:%d->",tmp);//DEBUG
  if(tmp>=CM->CodeSize)
  {
    printf("Invalid Code Address %d\n",tmp);
    exit(0);
  }
  tmp+=CM->CodeOffset;
  //printf("%d\n",tmp);//DEBUG
  return tmp;
}

ImportTabInd GetImportTabInd()
{
  TwoBytes x=GET2();
  if(x>=CM->ImportCount)
  {
    printf("Invalid Import Table %d\n",x);
    exit(0);
  }
  return CM->Import[x];
}

ImplGoalInd GetImplGoalInd()
{
  TwoBytes x=GET2();
  if(x>=CM->ImplGoalcount)
  {
    printf("Invalid Implication Goal %d\n",x);
    exit(0);
  }
  return CM->ImplGoaloffset+x;
}

HashTabInd GetHashTabInd()
{
  TwoBytes x=GET2();
  if(x>=CM->HashTabcount)
  {
    printf("Invalid Hash Table %d\n",x);
    exit(0);
  }
  return CM->HashTaboffset+x;
}

BvrTabInd GetBvrTabInd()
{
  TwoBytes x=GET2();
  if(x>=CM->BvrTabcount)
  {
    printf("Invalid Bound Variable Table %d\n",x);
    exit(0);
  }
  return CM->BvrTaboffset+x;
}

StringInd GetStringInd(int fd, struct Module_st* CMData)
{
  TwoBytes x=LK_FILE_GET2(fd);
  LK_ADJUST(x,CMData->StringsAdj,"String");
  return x;
}
