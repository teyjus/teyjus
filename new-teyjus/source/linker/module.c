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
	InitTGKinds();
	InitTLKinds();
	InitTTySkels();
	InitTGConsts();
	InitTLConsts();
	InitTHConsts();
	InitTStringSpaces();
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
	
	LoadTopGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadTopGConsts();
	LoadLConsts();
	LoadHConsts();
	
	LoadStringSpaces();
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
	
	LoadGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadGConsts();
	LoadLConsts();
	LoadHConsts();
	
	LoadStringSpaces();
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
	
	LoadGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadGConsts();
	LoadLConsts();
	LoadHConsts();
	
	LoadStringSpaces();
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
	CM->Import=malloc(sizeof(ImportTabInd)*count);
	if(CM->Import==NULL)
	{
		printf("Malloc failure\n");
		exit(0);
	}
	
	if(!count)
	{
		CM->SegmentID=-1;
		return;
	}
	
	CM->SegmentID=NumSegs++;
	
	for(i=0;i<count;i++)
	{
		Name name;
		GetName(&name);
		LoadKindRNTable();
		LoadConstRNTable();
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
		LoadKindRNTable();
		LoadConstRNTable();
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
	PUTWORD(LINKCODE_VER);
	Name name;
	name.string=modname;
	name.size=strlen(modname)+1;
	PutName(name);
	WriteDependencies();
	WriteCodeSize();
	WriteKinds();
	WriteTySkels();
	WriteConsts();
	WriteStringSpaces();
	WriteImplGoals();
	WriteHashTabs();
	WriteBvrTabs();
	WriteAddCodeTable();
	PUT1(NumSegs);
	WriteImportTabs();
	WriteCode();
}

KindInd GetKindInd(){
	INT1 tmp=GET1();
	INT2 tmp2=GET2();
	return FindKindInd(tmp,tmp2);
}

KindInd FindKindInd(INT1 gl_flag,INT2 index)
{
	KindInd tmp;
	tmp.gl_flag=gl_flag;
	tmp.index=index;
	//printf("KindInd:(%d,%d)->",tmp.gl_flag,tmp.index);//DEBUG
	switch(tmp.gl_flag)
	{
		case LOCAL:
			if(tmp.index>=CM->LKindcount)
			{
				printf("Invalid Local Kind %d\n",index);
				exit(0);
			}
			tmp.index+=CM->LKindoffset;
			break;
			
		case GLOBAL:
			if(tmp.index>=CM->GKindcount)
			{
				printf("Invalid Global Kind %d\n",index);
				exit(0);
			}
			tmp=CM->GKind[tmp.index];
			
		case PERVASIVE:
			break;
			
		default:
			printf("Invalid Kind Flag %d\n",tmp.gl_flag);
			exit(0);
			break;
	}
	//printf("(%d,%d)\n",tmp.gl_flag,tmp.index);//DEBUG
	return tmp;
}

void PutKindInd(KindInd x)
{
	PUT1(x.gl_flag);
	PUT2(x.index);
}

ConstInd GetConstInd(){
	ConstInd tmp;
	tmp.gl_flag=GET1();
	tmp.index=GET2();
	//printf("ConstInd:(%d,%d)->",tmp.gl_flag,tmp.index);//DEBUG
	switch(tmp.gl_flag)
	{
		case LOCAL:
			if(tmp.index>=CM->LConstcount)
			{
				printf("Invalid Local Constant %d\n",tmp.index);
				exit(0);
			}
			tmp.index+=CM->LConstoffset;
			break;
		
		case HIDDEN:
			if(tmp.index>=CM->HConstcount)
			{
				printf("Invalid Hidden Constant %d\n",tmp.index);
				exit(0);
			}
			tmp.index+=CM->HConstoffset;
			break;
			
		case GLOBAL:
			if(tmp.index>=CM->GConstcount)
			{
				printf("Invalid Global Constant %d\n",tmp.index);
				exit(0);
			}
			tmp=CM->GConst[tmp.index];
			break;
			
		case PERVASIVE:
			break;
			
		default:
			printf("Invalid Constant Flag %d\n",tmp.gl_flag);
			exit(0);
			break;
	}
	//printf("(%d,%d)\n",tmp.gl_flag,tmp.index);//DEBUG
	return tmp;
}

void PutConstInd(ConstInd x)
{
	PUT1(x.gl_flag);
	PUT2(x.index);
}

TySkelInd GetTySkelInd(){
	TySkelInd tmp=GET2();
	//printf("TySkel:%d->",tmp);//DEBUG
	if(tmp>=CM->TySkelcount)
	{
		printf("Invalid Type Skeleton %d\n",tmp);
		exit(0);
	}
	tmp+=CM->TySkeloffset;
	//printf("%d\n",tmp);//DEBUG
	return tmp;
}

void PutTySkelInd(TySkelInd x)
{
	PUT2(x);
}

CodeInd GetCodeInd(){
	CodeInd tmp=GETWORD();
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

void PutCodeInd(CodeInd x)
{
	PUTWORD(x);
}

ImportTabInd GetImportTabInd()
{
	INT2 x=GET2();
	if(x>=CM->ImportCount)
	{
		printf("Invalid Import Table %d\n",x);
		exit(0);
	}
	return CM->Import[x];
}

ImplGoalInd GetImplGoalInd()
{
	INT2 x=GET2();
	if(x>=CM->ImplGoalcount)
	{
		printf("Invalid Implication Goal %d\n",x);
		exit(0);
	}
	return CM->ImplGoaloffset+x;
}

HashTabInd GetHashTabInd()
{
	INT2 x=GET2();
	if(x>=CM->HashTabcount)
	{
		printf("Invalid Hash Table %d\n",x);
		exit(0);
	}
	return CM->HashTaboffset+x;
}

BvrTabInd GetBvrTabInd()
{
	INT2 x=GET2();
	if(x>=CM->BvrTabcount)
	{
		printf("Invalid Bound Variable Table %d\n",x);
		exit(0);
	}
	return CM->BvrTaboffset+x;
}

StringSpaceInd GetStringSpaceInd()
{
	INT2 x=GET2();
	if(x>=CM->StringSpacecount)
	{
		printf("Invalid String %d\n",x);
		exit(0);
	}
	return CM->StringSpaceoffset+x;
}

void PutImplGoalInd(ImplGoalInd x)
{
	PUT2(x);
}

void PutHashTabInd(HashTabInd x)
{
	PUT2(x);
}

void PutBvrTabInd(BvrTabInd x)
{
	PUT2(x);
}

void PutStringSpaceInd(StringSpaceInd x)
{
	PUT2(x);
}

void PutImportTabInd(ImportTabInd x)
{
	PUT2(x);
}
