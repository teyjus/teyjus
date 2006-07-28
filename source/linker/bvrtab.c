#include "vector.h"
#include "file.h"
#include "datatypes.h"
#include "module.h"
#include "code.h"
//////////////////////////////////////////////////////
//BvrTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 index;
	CodeInd addr;
}BvrTabEnt;

struct Vector BvrTabs;

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry);
void LoadBvrTab(struct Vector* BvrTab);
void WriteBvrTab(struct Vector* BvrTab);

void InitTBvrTabs()
{
	InitVec(&BvrTabs,32,sizeof(struct Vector));
}

void LoadBvrTabs()
{
	int i;
	INT2 count=CM->BvrTabcount=GET2();
	int offset=CM->BvrTaboffset=Extend(&BvrTabs,(int)count);
	struct Vector* tmp=(struct Vector*)Fetch(&BvrTabs,offset);
	for(i=0;i<count;i++)
	{
		LoadBvrTab(tmp+i);
	}
}

void LoadBvrTab(struct Vector* BvrTab)
{
	int j;
	INT1 count=GET1();
	InitVec(BvrTab,(int)count,sizeof(BvrTabEnt));
	Extend(BvrTab,(int)count);
	BvrTabEnt* tmp=(BvrTabEnt*)Fetch(BvrTab,0);
	for(j=0;j<count;j++)
	{
		tmp[j].index=GET1();
		tmp[j].addr=GetCodeInd();
	}
	
}

void WriteBvrTabs()
{
	int i;
	PUT2(BvrTabs.numEntries);
	struct Vector* tmp=Fetch(&BvrTabs,0);
	for(i=0;i<BvrTabs.numEntries;i++)
	{
		WriteBvrTab(tmp+i);
	}
}

void WriteBvrTab(struct Vector* BvrTab)
{
	int j;
	INT1 count=BvrTab->numEntries;
	PUT1(count);
	BvrTabEnt* tmp=(BvrTabEnt*)Fetch(BvrTab,0);
	for(j=0;j<count;j++)
	{
		PUT1(tmp[j].index);
		PutCodeInd(tmp[j].addr);
	}
}

int BvrTabSearch(struct Vector* BvrTab,INT1 index)
{
	BvrTabEnt* tmp=Fetch(BvrTab,0);
	int i;
	for(i=0;i<BvrTab->numEntries;i++)
	{
		if(tmp[i].index==index)
			return i;
	}
	return -1;
}

void MergeBvrTabs(BvrTabInd a, BvrTabInd b,INT1 n)
{
	struct Vector* pa=(struct Vector*)Fetch(&BvrTabs,a);
	struct Vector* pb=(struct Vector*)Fetch(&BvrTabs,b);
	
	int size=pb->numEntries;
	int i,j;
	BvrTabEnt* tmpa=(BvrTabEnt*)Fetch(pa,0);
	BvrTabEnt* tmpb=(BvrTabEnt*)Fetch(pb,0);
	for(i=0;i<size;i++)
	{
		j=BvrTabSearch(pa,tmpb[i].index);
		if(j!=-1)
		{
			tmpa[j].addr=MergeSubSequence(tmpa[j].addr,tmpb[i].addr,n);
		}
		else
		{
			BVR_AddEntry(pa,tmpb+i);
			tmpa=(BvrTabEnt*)Fetch(pa,0);
		}
	}
}

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry)
{
	BvrTabEnt* new=Fetch(BvrTab,Extend(BvrTab,1));
	new->index=entry->index;
	new->addr=entry->addr;
}
