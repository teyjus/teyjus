
//////////////////////////////////////////////////////
//TySkel Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT1 type;
	INT2 value;
	TTySkel_t* args;
}TTySkel_t;

#define ARROW 0
#define PKIND 1
#define LKIND 2
#define GKIND 3
#define VARIABLE 4

typedef struct{
	int entries;
	int size;
	TTySkel_t* entry;
}TySkel_Vec;

TySkel_Vec TySkels;

void InitTTySkels()
{
	TySkels.entries=0;
	TySkels.size=128;
	TySkels.entry=malloc(TySkels.size*sizeof(TTySkel_t));
	if(TySkels.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTTySkels(int count)
{
	int tmp=TySkels.entries;
	TySkels.entries=TySkels.entries+count;
	if(TySkels.entries>TySkels.size)
	{
		do{
			TySkels.size*=2;
		}while(TySkels.entries>TySkels.size)
		
			TySkels.entry=(TTySkel_t*)realloc((void*)TySkels.entry,TySkels.size*sizeof(TTySkel_t));
			if(TySkels.entry==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
	}
	return tmp;
}

LTySkel_t* AllocateLTySkels(int count)
{
	LTySkel_t* tmp=(LTySkel_t*)malloc(count*sizeof(LTySkel_t));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	return tmp;
}

void LoadTySkels()
{
	int count=CM.TySkelcount=GET1();
	int offset=CM.TySkeloffset=AllocateTTySkels(count);
	//CM.TySkel=AllocateLTySkels(count);
	for(int i=0;i<count;i++)
	{
		LoadTySkel(offset+i);
	}
}

LTySkel_t LoadTySkel(int i)
{
	TySkels.entry[i]=GetTySkel();
}

TTySkel_t GetTySkel()
{
	int i;
	TTySkel_t tmp;
	tmp.args=NULL;
	tmp.type=GET1();
	switch(tmp.type)
	{
		case ARROW:
			tmp.value=(INT2)GET1();
			tmp.args=malloc(tmp.value*sizeof(TTySkel_t));
			if(tmp.args==NULL)
			{
				perror("Memory Allocation Failed");
				exit(0);
			}
			for(i=0;i<tmp.value;i++)
			{
				tmp.args[i]=GetTySkel();
			}
			break;
		
		case PKIND:
			tmp.value=GET2();
			break;
			
		case LKIND:
			tmp.value=GetLKindInd();
			break;
			
		case GKIND:
			tmp.value=GetGKindInd();
			break;
			
		case VARIABLE:
			tmp.value=(INT2)GET1();
			break;
			
		default:
			perror("Error, Invalid Type Head");
			exit(0);
	}
	return tmp;
}

void WriteTySkels()
{
	PUT1(TySkels.entries);
	for(int i=0;i<TySkels.entries;i++)
	{
		WriteTySkel(i);
	}
}

void WriteTySkel(i)
{
	PutTySkel(TySkels.entry[i]);
}

void PutTySkel(TTySkel_t tyskel)
{
	INT1 x=tyskel.type;
	int i;
	PUT1(x);
	switch(x)
	{
		case ARROW:
			PUT1((INT1)tyskel.value);
			for(i=0;i<tmp.value;i++)
			{
				PutTySkel(tyskel.args[i]);
			}
			break;
		
		case PKIND:
		case LKIND:
		case GKIND:
			PUT2(tyskel.value);
			break;
			
		case VARIABLE:
			PUT1((INT1)tyskel.value);
			break;
			
		default:
			perror("Error, Invalid Type Head");
			exit(0);
	}
}