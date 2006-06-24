#include <stdio.h>
#include <stdlib.h>
#include "module.h"
#include "rename.h"
#include "vector.h"
#include "tree.h"
#include "file.h"

struct Vector ConstRNTab;
struct Tree ConstRNTree;

void LoadConstRNTabEnt(int i);

void LoadConstRNTable()
{
	if(ConstRNTree.root!=NULL)
	{
		printf("Const renaming table should be empty.\n");
		exit(0);
	}
	Destroy(&ConstRNTab);
	INT2 size=GET2();
	InitVec(&ConstRNTab,(int)size,sizeof(ConstInd));
	Extend(&ConstRNTab,(int)size);
	int i;
	for(i=0;i<size;i++)
	{
		LoadConstRNTabEnt(i);
	}
}

void LoadConstRNTabEnt(int i)
{
	ConstInd* tmp=(ConstInd*)Fetch(&ConstRNTab,i);
	
	Name name;
	GetName(&name);
	*tmp=GetConstInd();
	if(i!=Add(&ConstRNTree,name.string))
	{
		printf("Duplicate name in rename table\n");
		exit(0);
	}
	
	Clear(name);
}

ConstInd RenameConst(Name name)
{
	int tmp=Remove(&ConstRNTree,name.string);
	if(-1==tmp)
	{
		printf("Unrecognized global constant %s\n",name.string);
		exit(0);
	}
	else
	{
		return *(ConstInd*)Fetch(&ConstRNTab,tmp);
	}
}


struct Vector KindRNTab;
struct Tree KindRNTree;

void LoadKindRNTabEnt(int i);

void LoadKindRNTable()
{
	if(KindRNTree.root!=NULL)
	{
		printf("Kind renaming table should be empty.\n");
		exit(0);
	}
	Destroy(&KindRNTab);
	INT2 size=GET2();
	InitVec(&KindRNTab,(int)size,sizeof(KindInd));
	Extend(&KindRNTab,(int)size);
	int i;
	for(i=0;i<size;i++)
	{
		LoadKindRNTabEnt(i);
	}
}

void LoadKindRNTabEnt(int i)
{
	KindInd* tmp=(KindInd*)Fetch(&KindRNTab,i);
	
	Name name;
	GetName(&name);
	*tmp=GetKindInd();
	
	if(i!=Add(&KindRNTree,name.string))
	{
		printf("Duplicate name in rename table\n");
		exit(0);
	}
	
	Clear(name);
}

KindInd RenameKind(Name name)
{
	int tmp=Remove(&KindRNTree,name.string);
	if(-1==tmp)
	{
		printf("Unrecognized global kind %s\n",name.string);
		exit(0);
	}
	else
	{
		return *(KindInd*)Fetch(&KindRNTab,tmp);
	}
}

void InitRNTables()
{
	InitVec(&ConstRNTab,0,sizeof(ConstInd));
	InitTree(&ConstRNTree);
	InitVec(&KindRNTab,0,sizeof(KindInd));
	InitTree(&KindRNTree);
}
