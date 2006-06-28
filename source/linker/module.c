#include <stdlib.h>
#include <stdio.h>
#include "module.h"

#define BC_VER 3

// void LoadAccModule(char* modname);
// void LoadAccModules();
// void LoadImpModule(char* modname);
// void LoadImpModules();
// void InitAll();

// void InitAll()
// {
// 	CM=malloc(sizeof(Module));
// 	if(CM==NULL)
// 	{
// 		perror("Memory Allocation Failed");
// 		exit(0);
// 	}
// 	CM->parent=NULL;
// 	
// 	InitTGKinds();
// 	InitTLKinds();
// 	InitTTySkels();
// 	InitTGConsts();
// 	InitTLConsts();
// 	InitTHConsts();
// 	InitTStringSpaces();
// 	InitTImplGoals();
// 	InitTHashTabs();
// 	InitTBvrTabs();
// 	InitTCode();
// 	InitTImportTabs();
// }

// void LoadTopModule(char* modname)
// {
// 	PushInput(modname);
// 	CheckBytecodeVersion();
// 	CheckModuleName(modname);
// 	
// 	LoadStringSpaceSize();
// 	LoadTySkelSpace();
// 	GETWORD(); //Other Header Info?
// 	LoadCodeSize();
// 	
// 	LoadFindCodeFun();
// 	
// 	LoadTopGKinds();
// 	LoadLKinds();
// 	
// 	LoadTySkels();
// 	
// 	LoadTopGConsts();
// 	LoadLConsts();
// 	LoadHConsts();
// 	
// 	GET1();  //Clause segments
// 	
// 	LoadStringSpaces();
// 	LoadImplGoals();
// 	
// 	LoadHashTabs();
// 	LoadBvrTabs();
// 	
// 	LoadImpModules();
// 	
// 	LoadCode();
// 	
// 	NewImportTab();
// 	
// 	LoadAccModules();
// 	
// 	PopInput();
// }

// void LoadImpModule(char* modname)
// {
// 	PushInput(modname);
// 	CheckBytecodeVersion();
// 	CheckModuleName(modname);
// 	
// 	LoadStringSpaceSize();
// 	LoadTySkelSpace();
// 	GETWORD(); //Other Header Info?
// 	LoadCodeSize();
// 	
// 	LoadFindCodeFun();
// 	
// 	LoadGKinds();
// 	LoadLKinds();
// 	
// 	LoadTySkels();
// 	
// 	LoadGConsts();
// 	LoadLConsts();
// 	LoadHConsts();
// 	
// 	GET1();  //Clause segments
// 	
// 	LoadStringSpaces();
// 	LoadImplGoals();
// 	
// 	LoadHashTabs();
// 	LoadBvrTabs();
// 	
// 	LoadImpModules();
// 	
// 	LoadCode();
// 	
// 	NewImportTab();
// 	
// 	LoadAccModules();
// 	
// 	PopInput();
// }
// 
// void LoadAccModule(char* modname)
// {
// 	PushInput(modname);
// 	CheckBytecodeVersion();
// 	CheckModuleName(modname);
// 	
// 	LoadStringSpaceSize();
// 	LoadTySkelSpace();
// 	GETWORD(); //Other Header Info?
// 	LoadCodeSize();
// 	
// 	LoadFindCodeFun();
// 	
// 	LoadGKinds();
// 	LoadLKinds();
// 	
// 	LoadTySkels();
// 	
// 	LoadGConsts();
// 	LoadLConsts();
// 	LoadHConsts();
// 	
// 	GET1();  //Clause segments
// 	
// 	LoadStringSpaces();
// 	LoadImplGoals();
// 	
// 	LoadHashTabs();
// 	LoadBvrTabs();
// 	
// 	LoadImpModules();
// 	
// 	LoadCode();
// 	
// 	ExtImportTab();
// 	
// 	LoadAccModules();
// 	
// 	PopInput();
// }
// 
// void LoadImpModules()
// {
// 	Module* tmp;
// 	int count=GET1();
// 	int i;
// 	
// 	for(i=0;i<count;i++)
// 	{
// 		tmp=malloc(sizeof(Module));
// 		if(tmp==NULL)
// 		{
// 			perror("Memory Allocation Failed");
// 			exit(0);
// 		}
// 		tmp->parent=CM;
// 		CM=tmp;
// 		Name name=GetName();
// 		LoadConstRNTable();
// 		LoadKindRNTable();
// 		LoadImpModule(name.string);
// 		CM=CM->parent;
// 		free(tmp);
// 		RestoreImportTab(CM->ImportTab);
// 	}
// }
// 
// void LoadAccModules()
// {
// 	Module* tmp;
// 	int count=GET1();
// 	int i;
// 	
// 	for(i=0;i<count;i++)
// 	{
// 		tmp=malloc(sizeof(Module));
// 		if(tmp==NULL)
// 		{
// 			perror("Memory Allocation Failed");
// 			exit(0);
// 		}
// 		
// 		tmp->parent=CM;
// 		CM=tmp;
// 		
// 		Name name=GetName();
// 		LoadConstRNTable();
// 		LoadKindRNTable();
// 		LoadAccModule(name.string);
// 		
// 		CM=CM->parent;
// 		free(tmp);
// 	}
// }

void CheckBytecodeVersion()
{
	if(GET1()!=BC_VER)
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

void LoadFindCodeFun()
{
	CM->FindCodeFun=GET1();
}

// void WriteAll()
// {
// 	WriteDependencies();
// 	WriteTGKinds();
// 	WriteTLKinds();
// 	WriteTTySkels();
// 	WriteTGConsts();
// 	WriteTLConsts();
// 	WriteTHConsts();
// 	WriteTStringSpaces();
// 	WriteTImplGoals();
// 	WriteTHashTabs();
// 	WriteTBvrTabs();
// 	WriteTImportTabs();
// 	WriteTCode();
// }

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
	printf("KindInd:(%d,%d)->",tmp.gl_flag,tmp.index);
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
	printf("(%d,%d)\n",tmp.gl_flag,tmp.index);
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
	printf("ConstInd:(%d,%d)->",tmp.gl_flag,tmp.index);
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
	printf("(%d,%d)\n",tmp.gl_flag,tmp.index);
	return tmp;
}

void PutConstInd(ConstInd x)
{
	PUT1(x.gl_flag);
	PUT2(x.index);
}

TySkelInd GetTySkelInd(){
	TySkelInd tmp=GET2();
	printf("TySkel:%d->",tmp);
	if(tmp>=CM->TySkelcount)
	{
		printf("Invalid Type Skeleton %d\n",tmp);
		exit(0);
	}
	tmp+=CM->TySkeloffset;
	printf("%d\n",tmp);
	return tmp;
}

void PutTySkelInd(TySkelInd x)
{
	PUT2(x);
}

CodeInd GetCodeInd(){
	CodeInd tmp=GETWORD();
	printf("CodeInd:%d->",tmp);
	if(tmp>=CM->CodeSize)
	{
		printf("Invalid Code Address %d\n",tmp);
		exit(0);
	}
	tmp+=CM->CodeOffset;
	printf("%d\n",tmp);
	return tmp;
}

void PutCodeInd(CodeInd x)
{
	PUTWORD(x);
}
