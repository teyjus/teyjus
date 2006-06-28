#include <stdio.h>
#include <stdlib.h>
#include "../tables/instructions.h"
#include "code.h"
#include "module.h"
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
//////////////////////////////////////////////////////
//Code Load and Write Code
//////////////////////////////////////////////////////
struct BCFloat{
	INT4 mantissa;
	INT4 exponent;
};

struct Vector Code;
struct Vector Floats;

INT4 GetFloat();
void PutFloat(INT4);

CodeInd MergeSequence(CodeInd younger, CodeInd older, INT1 n);
void MergeTerm(CodeInd younger, CodeInd older,INT1 n);


void InitTCodes()
{
	InitVec(&Code,1024,sizeof(char));
	InitVec(&Floats,32,sizeof(struct BCFloat));
}

void LoadCode()
{
	int i,j;
	int offset=CM->CodeOffset;
	int size=CM->CodeSize;
	INT1* code=(INT1*)Fetch(&Code,offset);
	ConstInd tmpIndex;
	
	INT1 opcode=-1;
	INSTR_InstrCategory instrCat=INSTR_CAT_X;
	INSTR_OperandType* opType=NULL;
	
	int argid=0;
	for(i=0;i<size;)
	{
		j=i;
		opcode=code[j++]=GET1();
		if(opcode==call)
		{
			tmpIndex=GetConstInd();
			PushCall(tmpIndex,offset+i);
			i+=(INSTR_instrSize(opcode)*sizeof(WORD));
			continue;
		}
		
		instrCat=INSTR_InstrType(opcode);
		opType=INSTR_operandTypes(instrCat);
		argid=0;
		do{
			switch(opType[argid])
			{
				case INSTR_P:
					j++;
					break;
					
				case INSTR_R:
				case INSTR_E:
				case INSTR_N:
				case INSTR_I1:
				case INSTR_CE:
					*(INT1*)(code+j)=GET1();
					j+=sizeof(INT1);
					break;

				case INSTR_I2:
					*(INT2*)(code+j)=GET2();
					j+=sizeof(INT2);
					break;
					
				case INSTR_C:
					*(ConstInd*)(code+j)=GetConstInd();
					j+=sizeof(ConstInd);
					break;
					
				case INSTR_K:
					*(KindInd*)(code+j)=GetKindInd();
					j+=sizeof(KindInd);
					break;
					
				case INSTR_MT:
					*(ImportTabInd*)(code+j)=GetImportTabInd();
					j+=sizeof(ImportTabInd);
					break;
					
				case INSTR_IT:
					*(ImplGoalInd*)(code+j)=GetImplGoalInd();
					j+=sizeof(ImplGoalInd);
					break;
					
				case INSTR_HT:
					*(HashTabInd*)(code+j)=GetHashTabInd();
					j+=sizeof(HashTabInd);
					break;
					
				case INSTR_BVT:
					*(BvrTabInd*)(code+j)=GetBvrTabInd();
					j+=sizeof(BvrTabInd);
					break;
					
				case INSTR_S:
					*(StringSpaceInd*)(code+j)=GetStringSpaceInd();
					j+=sizeof(StringSpaceInd);
					break;
				
				case INSTR_L:
					*(CodeInd*)(code+j)=GetCodeInd();
					j+=sizeof(CodeInd);
					break;
					
				case INSTR_I:
					*(INT4*)(code+j)=GET4();
					j+=sizeof(INT4);
					break;

				case INSTR_F:
					*(INT4*)(code+j)=GetFloat();
					j+=sizeof(INT4);
					break;
					
				default:
				case INSTR_X:
					;			/* null instruction */
			}
			argid++;
		}
		while(opType[argid]!=INSTR_X);
		
		i+=INSTR_instrSize(opcode)*sizeof(WORD);
	}
}

void LoadCodeSize()
{
	int size=CM->CodeSize=GETWORD();
	CM->CodeOffset=Extend(&Code,size);
	
}

void WriteCodeSize()
{
	PUTWORD(Code.numEntries);
}

void WriteCode()
{
	int i,j;
	int size=Code.numEntries;
	char* code=Fetch(&Code,0);
	
	INT1 opcode=-1;
	INSTR_InstrCategory instrCat=INSTR_CAT_X;
	INSTR_OperandType* opType=NULL;
	
	int argid=0;
	for(i=0;i<size;)
	{
		j=i;
		
		opcode=code[j++];
		instrCat=INSTR_InstrType(opcode);
		opType=INSTR_operandTypes(instrCat);
		argid=-1;
		do{
			argid++;
			switch(opType[argid])
			{
				case INSTR_P:
					j++;
					break;
					
				case INSTR_R:
				case INSTR_E:
				case INSTR_N:
				case INSTR_I1:
				case INSTR_CE:
					PUT1(*(INT1*)(code+j));
					j+=sizeof(INT1);
					break;

				case INSTR_I2:
					PUT2(*(INT2*)(code+j));
					j+=sizeof(INT2);
					break;
					
				case INSTR_C:
					PutConstInd(*(ConstInd*)(code+j));
					j+=sizeof(ConstInd);
					break;
					
				case INSTR_K:
					PutKindInd(*(KindInd*)(code+j));
					j+=sizeof(KindInd);
					break;
					
				case INSTR_MT:
					PutImportTabInd(*(ImportTabInd*)(code+j));
					j+=sizeof(ImportTabInd);
					break;
					
				case INSTR_IT:
					PutImplGoalInd(*(ImplGoalInd*)(code+j));
					j+=sizeof(ImplGoalInd);
					break;
					
				case INSTR_HT:
					PutHashTabInd(*(HashTabInd*)(code+j));
					j+=sizeof(HashTabInd);
					break;
					
				case INSTR_BVT:
					PutBvrTabInd(*(BvrTabInd*)(code+j));
					j+=sizeof(BvrTabInd);
					break;
					
				case INSTR_S:
					PutStringSpaceInd(*(StringSpaceInd*)(code+j));
					j+=sizeof(StringSpaceInd);
					break;
				
				case INSTR_L:
					PutCodeInd(*(CodeInd*)(code+j));
					j+=sizeof(CodeInd);
					break;
					
				case INSTR_I:
					PUT4(*(INT4*)(code+j));
					j+=sizeof(INT4);
					break;

				case INSTR_F:
					PutFloat(*(INT4*)(code+j));
					j+=sizeof(INT4);
					break;
					
				default:
				case INSTR_X:
					;			/* null instruction */
			}
		}
		while(opType[argid]!=INSTR_X);
		
		i+=INSTR_instrSize(opcode)*sizeof(WORD);
	}
}

INT4 GetFloat()
{
	int i=Extend(&Floats,1);
	struct BCFloat* tmp=(struct BCFloat*)Fetch(&Floats,i);
	tmp->mantissa=GET4();
	tmp->exponent=GET4();
	return i;
}

void PutFloat(INT4 i)
{
	struct BCFloat* tmp=(struct BCFloat*)Fetch(&Floats,i);
	PUT4(tmp->mantissa);
	PUT4(tmp->exponent);
}

CodeInd MergeSubSequence(CodeInd a, CodeInd b, INT1 n)
{
	CodeInd c,d;
	INT1* code=(INT1*)Fetch(&Code,0);
	int trysize=INSTR_instrSize(try)*sizeof(WORD);
	int trustsize=INSTR_instrSize(trust)*sizeof(WORD);
	if(code[a]==try||code[a]==try_else)
	{
		c=a;
		if(code[c]==try)
			c+=trysize;
		else
			c+=trustsize;
		
		while(code[c]!=trust)
		{
			if(code[c]==retry)
				c+=trysize;
			else
				c+=trustsize;
		}
		
		code[c]=retry_else;
		c+=2+sizeof(CodeInd);
			
		if(code[b]==try||code[b]==try_else)
		{
			if(code[b]==try)
				code[b]=retry;
			else
				code[b]=retry_else;
			*(CodeInd*)(code+c)=b;
		}
		else
		{
			d=Extend(&Code,trustsize);
			code=(INT1*)Fetch(&Code,0);
			*(CodeInd*)(code+c)=d;
			*(code+d++)=trust;
			*(code+d++)=n;
			*(CodeInd*)(code+d)=b;
		}
		return a;
	}
	else
	{
		if(code[b]==try||code[b]==try_else)
		{
			if(code[b]==try)
				code[b]=retry;
			else
				code[b]=retry_else;
			
			c=Extend(&Code,trustsize);
			code=Fetch(&Code,c);
			*(code++)=try_else;
			*(code++)=n;
			*(CodeInd*)code=a;
			code+=sizeof(CodeInd);
			*(CodeInd*)code=b;
			return c;
		}
		else
		{
			c=Extend(&Code,trysize+trustsize);
			code=Fetch(&Code,c);
			*(code)=try;
			*(code+1)=n;
			*(CodeInd*)(code+2)=a;
			code+=trysize;
			*(code)=trust;
			*(code+1)=n;
			*(CodeInd*)(code+2)=b;
			return c;
		}
	}
}

//Append definition at older to definition at younger.
CodeInd MergeDefs(CodeInd older, CodeInd younger)
{
	INT1* code=Fetch(&Code,0);
	CodeInd c;
	int n;
	
	//Assert switch_on_reg for younger.
	if(code[younger]!=switch_on_reg)
	{
		printf("Invalid definition of older global predicate.");
		exit(0);
	}
	
	//Get predicate arity.
	c=*(CodeInd*)(code+younger++);
	n=code[c+1];
	
	//Get procedure address
	younger=*(CodeInd*)(code+younger+sizeof(CodeInd));
	
	//Check for switch_on_reg for older
	if(code[older]==switch_on_reg)
	{
		younger=MergeSequence(younger,*(CodeInd*)(code+older+2),n);
		code=Fetch(&Code,older+2);
		*(CodeInd*)code=younger;
		return older;
	}
	else
	{
		return MergeSequence(younger,older,n);
	}
}

//Append definition at older to definition at younger.
CodeInd MergeSequence(CodeInd younger, CodeInd older, INT1 n)
{
	CodeInd c;
	INT1* code=Fetch(&Code,0);
	int tmesize=INSTR_instrSize(try_me_else)*sizeof(WORD);
	if(code[younger]==try_me_else)
	{
		c=*(CodeInd*)(code+younger+2);
		while(c==retry_me_else)
			c=*(CodeInd*)(code+c+2);
		
		if(code[older]==try_me_else)
		{
			code[c]=retry_me_else;
			if((code[c+tmesize]==switch_on_term)&&(code[older+tmesize]==switch_on_term))
			{
				*(CodeInd*)(code+c+2)=*(CodeInd*)(code+older+2);
				MergeTerm(c+tmesize,older+tmesize,n);
				return younger;
			}
			else
			{
				*(CodeInd*)(code+c+2)=older;
				code[older]=retry_me_else;
				return younger;
			}
		}
		else
		{
			if((code[c+tmesize]==switch_on_term)&&(code[older]==switch_on_term))
			{
				MergeTerm(c+tmesize,older,n);
				return younger;
			}
			else
			{
				older-=tmesize;
				code[older]=trust_me;
				code[older+1]=n;
				code[c]=retry_me_else;
				*(CodeInd*)(code+c+2)=older;
			}
		}
	}
	else
	{
		if(code[older]==try_me_else)
		{
			younger-=tmesize;
			code[younger]=try_me_else;
			code[younger+1]=n;
				
			if((code[younger+tmesize]==switch_on_term)&&(code[older+tmesize]==switch_on_term))
			{
				*(CodeInd*)(code+younger+2)=*(CodeInd*)(code+older+2);
				MergeTerm(younger+tmesize,older+tmesize,n);
				return younger;
			}
			else
			{
				*(CodeInd*)(code+younger+2)=older;
				code[older]=retry_me_else;
				return younger;
			}
		}
		else
		{
			if((code[younger]==switch_on_term)&&(code[older]==switch_on_term))
			{
				MergeTerm(younger,older,n);
				return younger;
			}
			else
			{
				younger-=tmesize;
				older-=tmesize;
				code[younger]=try_me_else;
				code[younger+1]=n;
				*(CodeInd*)(code+younger+2)=older;
				code[older]=trust;
				code[older+1]=n;
				return younger;
			}
		}
	}
}

void MergeTerm(CodeInd younger, CodeInd older,INT1 n)
{
	INT1* code=(INT1*)Fetch(&Code,0);
	
	CodeInd c,d;
	//Combine variable subsequences.
	c=MergeSequence(*(CodeInd*)(code+younger++),*(CodeInd*)(code+older++),n);
	code=(INT1*)Fetch(&Code,0);
	*(CodeInd*)(code+younger)=c;
	
	//Combine constant subsequences.
	INT1 tmp1;
	younger+=sizeof(CodeInd);
	older+=sizeof(CodeInd);
	c=*(CodeInd*)(code+younger);
	d=*(CodeInd*)(code+older);
	if(code[d]==switch_on_constant)
	{
		if(code[c]==switch_on_constant)
		{
			tmp1=MergeHashTabs(*(HashTabInd*)(code+c+2),*(HashTabInd*)(code+d+2),n);
			code=(INT1*)Fetch(&Code,0);
			code[c+1]=tmp1;
		}
		else
		{
			*(CodeInd*)(code+younger)=d;
		}
	}
	
	//Combine List subsequences.
	younger+=sizeof(CodeInd);
	older+=sizeof(CodeInd);
	c=*(CodeInd*)(code+younger);
	d=*(CodeInd*)(code+older);
	if(code[d]!=fail)
	{
		if(code[c]!=fail)
		{
			d=MergeSubSequence(c,d,n);
			code=(INT1*)Fetch(&Code,0);
			
		}
		
		*(CodeInd*)(code+younger)=d;
	}
	
	//Combine Bvr subsequences
	INT2 tmp2;
	younger+=sizeof(CodeInd);
	older+=sizeof(CodeInd);
	c=*(CodeInd*)(code+younger);
	d=*(CodeInd*)(code+older);
	if(code[d]==switch_on_bvar)
	{
		if(code[c]==switch_on_bvar)
		{
			MergeBvrTabs(*(BvrTabInd*)(code+c+3),*(BvrTabInd*)(code+d+3),n);
			code=(INT1*)Fetch(&Code,0);
			if(*(INT2*)(code+c+1)<*(INT2*)(code+d+1))
				*(INT2*)(code+c+1)=*(INT2*)(code+d+1);
		}
		else
		{
			*(CodeInd*)(code+younger)=d;
		}
	}
}
