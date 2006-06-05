
//////////////////////////////////////////////////////
//HashTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	int location;
	INT1 tab_size;
	INT1 num_entries;
	HashTabEnt* table;
}THashTab_t;

typedef struct{
	ConstInd index;
	CodeInd addr;
}HashTabEnt;

typedef struct{
	int entries;
	int dead_entries;
	int size;
	THashTab_t* entry;
}HashTab_Vec;

HashTab_Vec HashTabs;

void InitTHashTabs();
int AllocateTHashTabs(int count);
LHashTab_t* AllocateLHashTabs(int count);
LHashTab_t LoadHashTab(int i);
void LoadHashTabs();
void WriteHashTab(int i);
void WriteHashTabs();

void InitTHashTabs()
{
	HashTabs.dead_entries=0;
	HashTabs.entries=0;
	HashTabs.size=128;
	HashTabs.entry=malloc(HashTabs.size*sizeof(THashTab_t));
	if(HashTabs.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTHashTabs(int count)
{
	int tmp=HashTabs.entries;
	HashTabs.entries=HashTabs.entries+count;
	if(HashTabs.entries>HashTabs.size)
	{
		do{
			HashTabs.size*=2;
		}while(HashTabs.entries>HashTabs.size)
		
		HashTabs.entry=(THashTab_t*)realloc((void*)HashTabs.entry,HashTabs.size*sizeof(THashTab_t));
		if(HashTabs.entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

// LHashTab_t* AllocateLHashTabs(int count)
// {
// 	LHashTab_t* tmp=(LHashTab_t*)malloc(count*sizeof(LHashTab_t));
// 	if(tmp==NULL)
// 	{
// 		perror("Memory Allocation Failed");
// 		exit(0);
// 	}
// 	
// 	return tmp;
// }

void LoadHashTabs()
{
	int count=CM->HashTabcount=GET1();
	int offset=CM->HashTaboffset=AllocateTHashTabs(count);
	//CM->HashTab=AllocateLHashTabs(count);
	for(int i=0;i<count;i++)
	{
		LoadHashTab(offset+i);
	}
}

LHashTab_t LoadHashTab(int i)
{
	HashTabs.entry[i].dead=0;
	HashTabs.entry[i].tab_size=GET1();
	int count = HashTabs.entry[i].num_entries=GET1();
	
	HashTabs.entry[i].table=malloc(count*sizeof(HashTabEnt));
	if(HashTabs.entry[i].table==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	for(int j=0;j<count;j++)
	{
		HashTabs.entry[i].table[j].index=GetConstInd();
		HashTabs.entry[i].table[j].addr=GetCodeInd();
	}
}

void WriteHashTabs()
{
	PUT1(HashTabs.entries-HashTabs.dead_entries);
	for(int i=0;i<HashTabs.entries;i++)
	{
		WriteHashTab(i);
	}
}

void WriteHashTab(i)
{
	if(HashTabs.entry[i].dead==0)
	{
		PUT1(HashTabs.entry[i].tab_size);
		PUT1(HashTabs.entry[i].num_entries);
		for(int j=0;j<HashTabs.entry[i].num_entries;j++)
		{
			PutConstInd(HashTabs.entry[i].table[j].index);
			PUT4(HashTabs.entry[i].table[j].addr);
		}
	}
}

int GetHashTabInd()
{
	return CM->HashTaboffset+GET1();
}

int MergeHashTabs(int a, int b)
{
	HashTabs.entry[b].dead=1;
	THashTab_t tmp;
	tmp.num_entries=HashTabs.entry[i].num_entries+HashTabs.entry[i].num_entries;
	//TODO
}