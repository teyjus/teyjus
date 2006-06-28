#include "vector.h"
#include "datatypes.h"
#include "module.h"
#include "file.h"
//////////////////////////////////////////////////////
//HashTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	ConstInd index;
	CodeInd addr;
}HashTabEnt;

struct Vector HashTabs;

void LoadHashTab(struct Vector* HashTab);
void WriteHashTab(struct Vector* HashTab);
void HASH_AddEntry(struct Vector* HashTab, HashTabEnt* entry);

void InitTHashTabs()
{
	InitVec(&HashTabs,32,sizeof(struct Vector));
}

void LoadHashTabs()
{
	int i;
	INT2 count=CM->HashTabcount=GET2();
	int offset=CM->HashTaboffset=Extend(&HashTabs,(int)count);
	struct Vector* tmp=(struct Vector*)Fetch(&HashTabs,offset);
	for(i=0;i<count;i++)
	{
		LoadHashTab(tmp+i);
	}
}

void LoadHashTab(struct Vector* HashTab)
{
	int j;
	INT2 count=GET2();
	InitVec(HashTab,(int)count,sizeof(HashTabEnt));
	Extend(HashTab,(int)count);
	HashTabEnt* tmp=Fetch(HashTab,0);
	for(j=0;j<count;j++)
	{
		tmp[j].index=GetConstInd();
		tmp[j].addr=GetCodeInd();
	}
}

void WriteHashTabs()
{
	int i;
	PUT2(HashTabs.numEntries);
	struct Vector* tmp=(struct Vector*)Fetch(&HashTabs,0);
	for(i=0;i<HashTabs.numEntries;i++)
	{
		WriteHashTab(tmp+i);
	}
}

void WriteHashTab(struct Vector* HashTab)
{
	int i;
	INT2 count=HashTab->numEntries;
	PUT2(count);
	HashTabEnt* tmp=Fetch(HashTab,0);
	for(i=0;i<count;i++)
	{
		PutConstInd(tmp[i].index);
		PutCodeInd(tmp[i].addr);
	}
}

int HashTabSearch(struct Vector* HashTab, ConstInd x)
{
	HashTabEnt* tmp=Fetch(HashTab,0);
	int i;
	for(i=0;i<HashTab->numEntries;i++)
	{
		if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
			return i;
	}
	return -1;
}

INT1 MergeHashTabs(HashTabInd a, HashTabInd b,INT1 n)
{
	struct Vector* pa=(struct Vector*)Fetch(&HashTabs,a);
	struct Vector* pb=(struct Vector*)Fetch(&HashTabs,b);
	
	int size=pb->numEntries;
	int i,j;
	HashTabEnt* tmpa=(HashTabEnt*)Fetch(pa,0);
	HashTabEnt* tmpb=(HashTabEnt*)Fetch(pb,0);
	for(i=0;i<size;i++)
	{
		j=HashTabSearch(pa,tmpb[i].index);
		if(j!=-1)
		{
			tmpa[j].addr=MergeSubSequence(tmpa[j].addr,tmpb[i].addr,n);
		}
		else
		{
			HASH_AddEntry(pa,tmpb+i);
			tmpa=(HashTabEnt*)Fetch(pa,0);
		}
	}
	Destroy(pb);
	return pa->numEntries;
}

void MergeFindCodeTabs(struct Vector* pa, struct Vector* pb)
{
	int size=pb->numEntries;
	int i,j;
	HashTabEnt* tmpa=(HashTabEnt*)Fetch(pa,0);
	HashTabEnt* tmpb=(HashTabEnt*)Fetch(pb,0);
	for(i=0;i<size;i++)
	{
		j=HashTabSearch(pa,tmpb[i].index);
		if(j!=-1)
		{
			tmpa[j].addr=MergeDefs(tmpa[j].addr,tmpb[i].addr);
		}
		else
		{
			HASH_AddEntry(pa,tmpb+i);
			tmpa=(HashTabEnt*)Fetch(pa,0);
		}
	}
}

void HASH_AddEntry(struct Vector* HashTab, HashTabEnt* entry)
{
	HashTabEnt* new=Fetch(HashTab,Extend(HashTab,1));
	new->index=entry->index;
	new->addr=entry->addr;
}
