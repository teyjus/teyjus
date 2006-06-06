
//////////////////////////////////////////////////////
//StringSpace Load and Write Code
//////////////////////////////////////////////////////
typedef Name TStringSpace_t;

typedef struct{
	int entries;
	int size;
	TStringSpace_t* entry;
}StringSpace_Vec;

StringSpace_Vec StringSpaces;

void InitTStringSpaces();
int AllocateTStringSpaces(int count);
LStringSpace_t* AllocateLStringSpaces(int count);
LStringSpace_t LoadStringSpace(int i);
void LoadStringSpaces();
void WriteStringSpace(int i);
void WriteStringSpaces();
void LoadStringSpaceSize();

int StringSpaceSize=0;

void InitTStringSpaces()
{
	StringSpaces.entries=0;
	StringSpaces.size=128;
	StringSpaces.entry=malloc(StringSpaces.size*sizeof(TStringSpace_t));
	if(StringSpaces.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTStringSpaces(int count)
{
	int tmp=StringSpaces.entries;
	StringSpaces.entries=StringSpaces.entries+count;
	if(StringSpaces.entries>StringSpaces.size)
	{
		do{
			StringSpaces.size*=2;
		}while(StringSpaces.entries>StringSpaces.size)
		
		StringSpaces.entry=(TStringSpace_t*)realloc((void*)StringSpaces.entry,StringSpaces.size*sizeof(TStringSpace_t));
		if(StringSpaces.entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

LStringSpace_t* AllocateLStringSpaces(int count)
{
	LStringSpace_t* tmp=(LStringSpace_t*)malloc(count*sizeof(LStringSpace_t));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return tmp;
}

Name GetName()
{
	Name tmp;
	int size=StringSpaces.entry[i].size=GETWORD();
	tmp.string=malloc(size+1);
	if(tmp.string==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	for(i=0;i<size;i++)
	{
		tmp.string[i]=GET1();
	}
	tmp.string[size]='\0';
	return tmp;
}

void LoadStringSpaceSize()
{
	StringSpaceSize+=GETWORD();
}

void LoadStringSpace(int i)
{
	StringSpaces.entry[i]=GetName();
}


void LoadStringSpaces()
{
	int count=CM->StringSpacecount=GET2();
	CM->StringSpaceoffset=AllocateTStringSpaces(count);
	for(int i=0;i<count;i++)
	{
		LoadStringSpace(i);
	}
}

void WriteStringSpace(int i)
{
	int i;
	int size=StringSpaces.entry[i].size;
	PUTWORD(size);
	
	for(i=0;i<size;i++)
	{
		PUT1(StringSpaces.entry[i].string[i]);
	}
}

void WriteStringSpaces()
{
	PUT2(StringSpaces.entries);
	for(int i=0;i<StringSpaces.entries;i++)
	{
		WriteStringSpace(i);
	}
}
