
//////////////////////////////////////////////////////
//ImplGoal Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
	INT2 num_ext_pred;
	ConstInd* indexes;
	INT1 find_code_fun;
	INT1 tab_size;
	INT2 num_entries;
	HashTabEnt* table;
}TImplGoal_t;

typedef struct{
	ConstInd index;
	int addr;
}HashTabEnt;

typedef struct{
	int entries;
	int size;
	TImplGoal_t* entry;
}ImplGoal_Vec;

ImplGoal_Vec ImplGoals;

void InitTImplGoals();
int AllocateTImplGoals(int count);
LImplGoal_t* AllocateLImplGoals(int count);
LImplGoal_t LoadImplGoal(int i);
void LoadImplGoals();
void WriteImplGoal(int i);
void WriteImplGoals();

void InitTImplGoals()
{
	ImplGoals.entries=0;
	ImplGoals.size=128;
	ImplGoals.entry=malloc(ImplGoals.size*sizeof(TImplGoal_t));
	if(ImplGoals.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTImplGoals(int count)
{
	int tmp=ImplGoals.entries;
	ImplGoals.entries=ImplGoals.entries+count;
	if(ImplGoals.entries>ImplGoals.size)
	{
		do{
			ImplGoals.size*=2;
		}while(ImplGoals.entries>ImplGoals.size)
		
		ImplGoals.entry=(TImplGoal_t*)realloc((void*)ImplGoals.entry,ImplGoals.size*sizeof(TImplGoal_t));
		if(ImplGoals.entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

// LImplGoal_t* AllocateLImplGoals(int count)
// {
// 	LImplGoal_t* tmp=(LImplGoal_t*)malloc(count*sizeof(LImplGoal_t));
// 	if(tmp==NULL)
// 	{
// 		perror("Memory Allocation Failed");
// 		exit(0);
// 	}
// 	
// 	return tmp;
// }

void LoadImplGoals()
{
	int count=CM->ImplGoalcount=GET1();
	int offset=CM->ImplGoaloffset=AllocateTImplGoals(count);
	//CM->ImplGoal=AllocateLImplGoals(count);
	for(int i=0;i<count;i++)
	{
		LoadImplGoal(offset+i);
	}
}

LImplGoal_t LoadImplGoal(int i)
{
	int count=ImplGoals.entry[i].num_ext_pred=GET2();
	
	ImplGoals.entry[i].indexes=malloc(count*sizeof(ConstInd));
	if(ImplGoals.entry[i].indexes==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	for(int j=0;j<count;j++)
	{
		ImplGoals.entry[i].indexes[j]=GetConstInd();
		//FlagDynamicPred(ImplGoals.entry[i].indexes[j]);
	}
	
	ImplGoals.entry[i].find_code_fun=GET1();
	ImplGoals.entry[i].tab_size=GET1();
	int count = ImplGoals.entry[i].num_entries=GET2();
	
	ImplGoals.entry[i].table=malloc(count*sizeof(HashTabEnt));
	if(ImplGoals.entry[i].table==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	for(int j=0;j<count;j++)
	{
		ImplGoals.entry[i].table[j].index=GetConstInd();
		ImplGoals.entry[i].table[j].addr=GetCodeInd();
	}
}

void WriteImplGoals()
{
	PUT1(ImplGoals.entries);
	for(int i=0;i<ImplGoals.entries;i++)
	{
		WriteImplGoal(i);
	}
}

void WriteImplGoal(i)
{
	int count=ImplGoals.entry[i].num_ext_pred;
	PUT2(count);
	
	for(int j=0;j<count;j++)
	{
		PutConstInd(ImplGoals.entry[i].indexes[j]);
	}
	PUT1(ImplGoals.entry[i].find_code_fun);
	PUT1(ImplGoals.entry[i].tab_size);
	int count = ImplGoals.entry[i].num_entries;
	PUT2(count);
	
	for(int j=0;j<count;j++)
	{
		PutConstInd(ImplGoals.entry[i].table[j].index);
		PutCodeInd(ImplGoals.entry[i].table[j].addr);
	}
}