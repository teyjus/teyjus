
#include <stdlib.h>
#include "tables/instructions.h"
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
	int opcode=-1;
	argcode
	INSTR_operand_type argtype=INSTR_X;
	int argid=0;
	while(offset<limit)
	{
		opcode=Code.entry[offset++]=GET1();
		argcode=INSTR_info[opcode];
		argid=0;
		do{
			argtype=INSTR_operand_types[argcode][argid++];
			switch(argtype)
			{
				case INSTR_P:
					offset++;
					break;
					
				case INSTR_R:
				case INSTR_E:
				case INSTR_N:
				case INSTR_I1:
				case INSTR_CE:
					*(INT1*)(Code.entry+offset)=GET1();
					offset+=sizeof(INT1);
					break;

				case INSTR_I2:
					*(INT2*)(Code.entry+offset)=GET2();
					offset+=sizeof(INT2);
					break;
					
				case INSTR_T:
					*(TySkelInd*)(Code.entry+offset)=GetTySkelInd();
					offset+=sizeof(TySkelInd);
					break;
					
				case INSTR_C:
					*(ConstInd*)(Code.entry+offset)=GetConstInd();
					offset+=sizeof(ConstInd);
					break;
					
				case INSTR_K:
					*(KindInd*)(Code.entry+offset)=GetKindInd();
					offset+=sizeof(KindInd);
					break;
					
				case INSTR_MT:
					*(ImportTabInd*)(Code.entry+offset)=GetImportTabInd();
					offset+=sizeof(ImportTabInd);
					break;
					
				case INSTR_IT:
					*(ImplTabInd*)(Code.entry+offset)=GetImplTabInd();
					offset+=sizeof(ImplTabInd);
					break;
					
				case INSTR_HT:
					*(HashTabInd*)(Code.entry+offset)=GetHashTabInd();
					offset+=sizeof(HashTabInd);
					break;
					
				case INSTR_BVT:
					*(BvrTabInd*)(Code.entry+offset)=GetBvrTabInd();
					offset+=sizeof(BvrTabInd);
					break;
					
				case INSTR_S:
				case INSTR_SL:
					*(StringSpaceInd*)(Code.entry+offset)=GetStringSpaceInd();
					offset+=sizeof(StringSpaceInd);
					break;
				
				case INSTR_L:
					*(CodeInd*)(Code.entry+offset)=GetCodeInd();
					offset+=sizeof(CodeInd);
					break;
					
				case INSTR_I:
					*(INT4*)(Code.entry+offset)=GET4();
					offset+=sizeof(INT4);
					break;

				case INSTR_F:
					*(LFloat*)(Code.entry+offset)=GetFloat();
					offset+=sizeof(LFloat);
					break;

				default:
				case INSTR_X:
					;			/* null instruction */
			}
		}
		while(argtype!=INSTR_X)
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
