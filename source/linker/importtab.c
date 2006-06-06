
//////////////////////////////////////////////////////
//ImportTab Load and Write Code
//////////////////////////////////////////////////////
struct ImportTabEnt_st{
	ConstInd index;
	CodeInd addr;
	struct ImportTabEnt_st* next;
	struct ImportTabEnt_st* last;
};

typedef struct ImportTabEnt_st ImportTabEnt;

struct ImportTab_Node{
	INT2 num_ext_pred;
	ConstInd* index;	//Next Clause Table
	INT1 find_code_fun;	//Find Code Fun.
	int tab_size;
	ImportTabEnt* tab_front;
	ImportTabEnt* tab_back;
	struct ImportTab_Node* next;
}

typedef struct ImportTab_Node TImportTab_t;

typedef struct{
	int size;
	TImportTab_t* front;
	TImportTab_t* back;
}ImportTab_List;

ImportTab_List ImportTabs;
TImportTab_t* CT;	//Import Table of CM.

void InitTImportTabs();
void NewImportTab();
void ExtImportTab();
void WriteImportTabs();

void LoadImportTab();
void LoadImportTabEnt();
void WriteImportTab(int i);

void RestoreImportTab(LImportTab_t Tab)
{
	CT=Tab;
}

void InitTImportTabs()
{
	ImportTabs.size=0;
	ImportTabs.front=NULL;
	ImportTabs.back=NULL;
}

void NewImportTab()
{
	TImportTab_t* tmp=malloc(sizeof(TImportTab_t));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	
	CT=tmp;
	CM->ImportTab=(LImportTab_t)tmp;
	
	int i;
	int count=tmp->num_ext_pred=GET2();
	tmp.index=malloc(count*sizeof(ConstInd));
	for(i=0;i<count;i++)
	{
		tmp.index[i]=GetConstInd();
	}
	
	tmp->tab_size=0;
	tmp->tab_front=NULL;
	tmp->tab_back=NULL;
	tmp->next=NULL;
	
	if(ImportTabs.front==NULL)
	{
		ImportTabs.front=tmp;
	}
	
	ImportTabs.back->next=tmp;
	ImportTabs.back=tmp;
	
	LoadImportTab();
}

void ExtImportTab()
{
	int i;
	int count=GET2();
	for(i=0;i<count;i++)
	{
		GetConstInd();
	}
	CM->ImportTab=(LImportTab_t)CT;
	LoadImportTab();
}

void LoadImportTab()
{
	int count=GET1();
	for(int i=0;i<count;i++)
	{
		LoadImportTabEnt();
	}
}

void LoadImportTabEnt()
{
	ConstInd index=GetConstInd();
	CodeAddr addr=GetCodeInd();
	ImportTabEnt* tmp=CT->tab_front;
	ImportTabEnt* tmp2=NULL;
	while(tmp!=NULL&&0>ConstIndCmp(index,tmp->index))
	{
		tmp=tmp->next;
	}
	
	if(tmp==NULL)
	{
		tmp=malloc(sizeof(ImportTabEnt));
		if(tmp==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
		CT->size++;
		
		tmp->last=CT->tab_back;
		CT->tab_back->next=tmp;
		CT->tab_back=tmp;
		tmp->next=NULL;
		
		tmp->index=index;
		tmp->addr=addr;
	}
	else if(0<ConstIndCmp(index,tmp->index))
	{
		tmp2=malloc(sizeof(ImportTabEnt));
		if(tmp2==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
		CT->size++;
		
		tmp2->last=tmp->last;
		tmp->last=tmp2;
		tmp2->last->next=tmp2;
		tmp2->next=tmp;
		
		tmp2->index=index;
		tmp2->addr=addr;
	}
	else
	{
		tmp->addr=MergeDefs(tmp->addr,addr);
	}
}
