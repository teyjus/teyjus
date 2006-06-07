
//////////////////////////////////////////////////////
//Code Load and Write Code
//////////////////////////////////////////////////////
typedef void TCode_t;

typedef struct{
	int entries;
	int size;
	TCode_t* entry;
}Code_Vec;

Code_Vec Code;

void InitTCode();
int AllocateTCode(int count);
void LoadCode();
void WriteCode();
void LoadCodeSize();

void InitTCodes()
{
	Code.entries=0;
	Code.size=128;
	Code.entry=malloc(Code.size*sizeof(TCode_t));
	if(Code.entry==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
}

int AllocateTCodes(int count)
{
	int tmp=Code.entries;
	Code.entries=Code.entries+count;
	if(Code.entries>Code.size)
	{
		do{
			Code.size*=2;
		}while(Code.entries>Code.size)
		
		Code.entry=(TCode_t*)realloc((void*)Code.entry,Code.size*sizeof(TCode_t));
		if(Code.entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

void LoadCode()
{
	int size=CM->Codesize;
	int offset=CM->offset;
	int limit=size+offset;
	int opcode
	while(offset<limit)
	{
		opcode=Code.entry[offset]=GET1();
		switch(opcode)
		{
			
		}
	}
}

void LoadCodeSize()
{
	int size=CM->Codesize=GET1();
	CM->Codeoffset=AllocateTCodes(size);
	
}

void WriteCode()
{
	PUT1(Codes.entries);
	for(int i=0;i<Codes.entries;i++)
	{
		PUT1(Code.entry[i]);
	}
}
