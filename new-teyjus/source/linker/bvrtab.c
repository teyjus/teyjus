
//////////////////////////////////////////////////////
//BvrTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 tab_size;
	INT1 num_entries;
	BvrTabEnt* table;
}TBvrTab_t;

typedef struct{
	INT1 index;
	CodeInd addr;
}BvrTabEnt;

typedef struct{
	int entries;
	int size;
	TBvrTab_t* entry;
}BvrTab_Vec;

BvrTab_Vec BvrTabs;

void InitTBvrTabs();
int AllocateTBvrTabs(int count);
LBvrTab_t* AllocateLBvrTabs(int count);
LBvrTab_t LoadBvrTab(int i);
void LoadBvrTabs();
void WriteBvrTab(int i);
void WriteBvrTabs();

void InitTBvrTabs()
{
	BvrTabs.entries=0;
	BvrTabs.size=128;
	BvrTabs.entry=malloc(BvrTabs.size*sizeof(TBvrTab_t));
	if(BvrTabs.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTBvrTabs(int count)
{
	int tmp=BvrTabs.entries;
	BvrTabs.entries=BvrTabs.entries+count;
	if(BvrTabs.entries>BvrTabs.size)
	{
		do{
			BvrTabs.size*=2;
		}while(BvrTabs.entries>BvrTabs.size)
		
			BvrTabs.entry=(TBvrTab_t*)realloc((void*)BvrTabs.entry,BvrTabs.size*sizeof(TBvrTab_t));
			if(BvrTabs.entry==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
	}
	return tmp;
}

// LBvrTab_t* AllocateLBvrTabs(int count)
// {
// 	LBvrTab_t* tmp=(LBvrTab_t*)malloc(count*sizeof(LBvrTab_t));
// 	if(tmp==NULL)
// 	{
// 		perror("Memory Allocation Failed");
// 		exit(0);
// 	}
// 	
// 	return tmp;
// }

void LoadBvrTabs()
{
	int count=CM->BvrTabcount=GET1();
	int offset=CM->BvrTaboffset=AllocateTBvrTabs(count);
	for(int i=0;i<count;i++)
	{
		LoadBvrTab(offset+i);
	}
}

void LoadBvrTab(int i)
{
	BvrTabs.entry[i].tab_size=GET1();
	int count = BvrTabs.entry[i].num_entries=GET1();
	BvrTabs.entry[i].table=malloc(count*sizeof(BvrTabEnt));
	if(BvrTabs.entry[i].table==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	for(int j=0;j<count;j++)
	{
		BvrTabs.entry[i].table[j].index=GET1();
		BvrTabs.entry[i].table[j].addr=GetCodeInd();
	}
	
}

void WriteBvrTabs()
{
	PUT1(BvrTabs.entries);
	for(int i=0;i<BvrTabs.entries;i++)
	{
		WriteBvrTab(i);
	}
}

void WriteBvrTab(i)
{
	PUT1(BvrTabs.entry[i].tab_size);
	int count=BvrTabs.entry[i].num_entries;
	PUT1(count);
	for(int j=0;j<count;j++)
	{
		PUT1(BvrTabs.entry[i].table[j].index);
		PUT4(BvrTabs.entry[i].table[j].addr);
	}
}