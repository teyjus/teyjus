#include "rename.h"

struct ConstRNTabEnt{
	Name name;
	ConstInd index;
	struct ConstRNTabEnt* next;
	struct ConstRNTabEnt* last;
};

struct ConstRNTabEnt* ConstRNTab;

void LoadConstRNTable()
{
	if(ConstRNTab!=NULL)
	{
		printf("Const renaming table should be empty.");
		exit(0);
	}
	
	int size=GET1();
	int i;
	for(i=0;i<size;i++)
	{
		LoadConstRNTabEnt();
	}
}

void LoadConstRNTabEnt()
{
	struct ConstRNTabEnt* ent=malloc(sizeof(struct ConstRNTabEnt));
	if(ent==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	ent->string=GetName();
	ent->index=GetConstInd();
	ent->next=ConstRNTab;
	ConstRNTab->last=ent;
	ConstRNTab=ent;
}

ConstInd RenameConst(Name name)
{
	ConstInt ret;
	struct ConstRNTabEnt* tmp=ConstRNTab;
	while(tmp!=NULL)
	{
		if(strcmp(tmp.name.string,name.string)==0)
		{
			if(tmp.next!=NULL)
				tmp.next.last=tmp.last;
			
			if(tmp.last!=NULL)
			{
				tmp.last.next=tmp.next;
			}
			else
			{
				ConstRNTab=tmp.next;
			}
			
			ret=tmp.index;
			free(tmp);
			return ret;
		}
	}
	
	printf("Unable to find name in rename table");
	exit(0);
}


struct KindRNTabEnt{
	Name name;
	KindInd index;
	struct KindRNTabEnt* next;
	struct KindRNTabEnt* last;
};

struct KindRNTabEnt* KindRNTab;

void LoadKindRNTable()
{
	if(KindRNTab!=NULL)
	{
		printf("Kind renaming table should be empty.");
		exit(0);
	}
	
	int size=GET1();
	int i;
	for(i=0;i<size;i++)
	{
		LoadKindRNTabEnt();
	}
}

void LoadKindRNTabEnt()
{
	struct KindRNTabEnt* ent=malloc(sizeof(struct KindRNTabEnt));
	if(ent==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	ent->string=GetName();
	ent->index=GetKindInd();
	ent->next=KindRNTab;
	KindRNTab->last=ent;
	KindRNTab=ent;
}

KindInd RenameKind(Name name)
{
	KindInt ret;
	struct KindRNTabEnt* tmp=KindRNTab;
	while(tmp!=NULL)
	{
		if(strcmp(tmp.name.string,name.string)==0)
		{
			if(tmp.next!=NULL)
				tmp.next.last=tmp.last;
			
			if(tmp.last!=NULL)
			{
				tmp.last.next=tmp.next;
			}
			else
			{
				KindRNTab=tmp.next;
			}
			
			ret=tmp.index;
			free(tmp);
			return ret;
		}
	}
	
	printf("Unable to find name in rename table");
	exit(0);
}

