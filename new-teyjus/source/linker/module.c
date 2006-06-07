#include "module.h"

#define BC_VER 3

void LoadAccModule(char* modname);
void LoadAccModules();
void LoadImpModule(char* modname);
void LoadImpModules();
void InitAll();

void InitAll()
{
	CM=malloc(sizeof(Module));
	if(CM==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	CM->parent=NULL;
	
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
	PushInput(modname);
	CheckBytecodeVersion();
	CheckModuleName(modname);
	
	LoadStringSpaceSize();
	LoadTySkelSpace();
	GETWORD(); //Other Header Info?
	LoadCodeSize();
	
	LoadFindCodeFun();
	
	LoadTopGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadTopGConsts();
	LoadLConsts();
	LoadHConsts();
	
	GET1();  //Clause segments
	
	LoadStringSpaces();
	LoadImplGoals();
	
	LoadHashTabs();
	LoadBvrTabs();
	
	LoadImpModules();
	
	LoadCode();
	
	NewImportTab();
	
	LoadAccModules();
	
	PopInput();
}

void LoadImpModule(char* modname)
{
	PushInput(modname);
	CheckBytecodeVersion();
	CheckModuleName(modname);
	
	LoadStringSpaceSize();
	LoadTySkelSpace();
	GETWORD(); //Other Header Info?
	LoadCodeSize();
	
	LoadFindCodeFun();
	
	LoadGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadGConsts();
	LoadLConsts();
	LoadHConsts();
	
	GET1();  //Clause segments
	
	LoadStringSpaces();
	LoadImplGoals();
	
	LoadHashTabs();
	LoadBvrTabs();
	
	LoadImpModules();
	
	LoadCode();
	
	NewImportTab();
	
	LoadAccModules();
	
	PopInput();
}

void LoadAccModule(char* modname)
{
	PushInput(modname);
	CheckBytecodeVersion();
	CheckModuleName(modname);
	
	LoadStringSpaceSize();
	LoadTySkelSpace();
	GETWORD(); //Other Header Info?
	LoadCodeSize();
	
	LoadFindCodeFun();
	
	LoadGKinds();
	LoadLKinds();
	
	LoadTySkels();
	
	LoadGConsts();
	LoadLConsts();
	LoadHConsts();
	
	GET1();  //Clause segments
	
	LoadStringSpaces();
	LoadImplGoals();
	
	LoadHashTabs();
	LoadBvrTabs();
	
	LoadImpModules();
	
	LoadCode();
	
	ExtImportTab();
	
	LoadAccModules();
	
	PopInput();
}

void LoadImpModules()
{
	Module* tmp;
	int count=GET1();
	int i;
	
	for(i=0;i<count;i++)
	{
		tmp=malloc(sizeof(Module));
		if(tmp==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
		tmp->parent=CM;
		CM=tmp;
		Name name=GetName();
		LoadConstRNTable();
		LoadKindRNTable();
		LoadImpModule(name.string);
		CM=CM->parent;
		free(tmp);
		RestoreImportTab(CM->ImportTab);
	}
}

void LoadAccModules()
{
	Module* tmp;
	int count=GET1();
	int i;
	
	for(i=0;i<count;i++)
	{
		tmp=malloc(sizeof(Module));
		if(tmp==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
		
		tmp->parent=CM;
		CM=tmp;
		
		Name name=GetName();
		LoadConstRNTable();
		LoadKindRNTable();
		LoadAccModule(name.string);
		
		CM=CM->parent;
		free(tmp);
	}
}

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
	Name name=GetName();
	if(strcmp(modname,name.string)!=0)
	{
		perror("Module name mismatch");
		exit(0);
	}
}

void LoadFindCodeFun()
{
	CM->FindCodeFun=GET1();
}

void WriteAll()
{
	WriteDependencies();
	WriteTGKinds();
	WriteTLKinds();
	WriteTTySkels();
	WriteTGConsts();
	WriteTLConsts();
	WriteTHConsts();
	WriteTStringSpaces();
	WriteTImplGoals();
	WriteTHashTabs();
	WriteTBvrTabs();
	WriteTImportTabs();
	WriteTCode();
}
