#include <stdio.h>
#include <stdlib.h>
#include "../tables/instructions.h"
#include "code.h"
#include "module.h"
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "hashtab.h"
#include "bvrtab.h"

#define ASSERT(x,err) if(!(x)){printf("%s\n",err); exit(0);}
#define INSTR_LLen sizeof(int)
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

CodeInd MergeSequence(CodeInd younger, CodeInd older, Byte n);
void MergeTerm(CodeInd younger, CodeInd older,Byte n);

void InitTCode()
{
  LK_VECTOR_Init(&Code,1024,sizeof(char));
  LK_VECTOR_Init(&Floats,32,sizeof(struct BCFloat));
}

void LoadCode()
{
  int i,j;
  int offset=CM->CodeOffset;
  int size=CM->CodeSize;
  printf("Loading %d bytes of code to offset %d.\n",size,offset);//DEBUG
  Byte* code=(Byte*)LK_VECTOR_GetPtr(&Code,offset);
  ConstInd tmpIndex;
  
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  
  int argid=0;
  for(i=0;i<size;)
  {
    j=i;
    opcode=code[j++]=GET1();
    //printf("%d:\t%s\n",i,INSTR_instrName(opcode));//DEBUG
    if(opcode==call)
    {
      code[j]=GET1();
      tmpIndex=GetConstInd();
      PushCall(tmpIndex,offset+i,0);
      i+=(INSTR_instrSize(opcode)*sizeof(Word));
      continue;
    }
    else if(opcode==execute)
    {
      tmpIndex=GetConstInd();
      PushCall(tmpIndex,offset+i,1);
      i+=(INSTR_instrSize(opcode)*sizeof(Word));
      continue;
    }
    
    instrCat=INSTR_instrType(opcode);
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
          *(Byte*)(code+j)=GET1();
          j+=sizeof(Byte);
          break;

//         case INSTR_I2:
//           *(TwoBytes*)(code+j)=GET2();
//           j+=sizeof(TwoBytes);
//           break;
          
        case INSTR_C:
          *(ConstInd*)(code+j)=GetConstInd();
          j+=sizeof(ConstInd);
          break;
          
        case INSTR_K:
          *(KindInd*)(code+j)=GetKindInd(PeekInput(),CM);
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
          ;      /* null instruction */
      }
      argid++;
    }
    while(opType[argid]!=INSTR_X);
    i+=INSTR_instrSize(opcode)*sizeof(Word);
  }
}

void LoadCodeSize()
{
  int size=CM->CodeSize=(int)GETWord();
  CM->CodeOffset=LK_VECTOR_Grow(&Code,size);
  printf("Loaded Codesize = %d, got offset %d.\n",size,CM->CodeOffset);
}

void WriteCodeSize()
{
  PUTWord((Word)LK_VECTOR_Size(&Code));
}

void WriteCode()
{
  int i,j;
  int size=LK_VECTOR_Size(&Code);
  char* code=LK_VECTOR_GetPtr(&Code,0);
  
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  
  int argid=0;
  for(i=0;i<size;)
  {
    j=i;
    
    opcode=code[j++];
    instrCat=INSTR_instrType(opcode);
    opType=INSTR_operandTypes(instrCat);
    argid=-1;
    PUT1(opcode);
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
          PUT1(*(Byte*)(code+j));
          j+=sizeof(Byte);
          break;

//         case INSTR_I2:
//           PUT2(*(TwoBytes*)(code+j));
//           j+=sizeof(TwoBytes);
//           break;
          
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
          ;      /* null instruction */
      }
    }
    while(opType[argid]!=INSTR_X);
    
    i+=INSTR_instrSize(opcode)*sizeof(Word);
  }
}

void MakeCallName(CodeInd from, int exec_flag, ConstInd to)
{
  Byte* tmp=LK_VECTOR_GetPtr(&Code,from);
  if(exec_flag)
  {
    tmp[0]=execute_name;
    *(ConstInd*)(tmp+1)=to;
  }
  else
  {
    tmp[0]=call_name;
    *(ConstInd*)(tmp+2)=to;
  }
}
  
void MakeCall(CodeInd from, int exec_flag, CodeInd to)
{
  Byte* tmp=LK_VECTOR_GetPtr(&Code,from);
  if(exec_flag)
  {
    tmp[0]=execute;
    *(CodeInd*)(tmp+1)=to;
  }
  else
  {
    tmp[0]=call;
    *(CodeInd*)(tmp+2)=to;
  }
}


INT4 GetFloat()
{
  int i=LK_VECTOR_Grow(&Floats,1);
  struct BCFloat* tmp=(struct BCFloat*)LK_VECTOR_GetPtr(&Floats,i);
  tmp->mantissa=GET4();
  tmp->exponent=GET4();
  return i;
}

void PutFloat(INT4 i)
{
  struct BCFloat* tmp=(struct BCFloat*)LK_VECTOR_GetPtr(&Floats,i);
  PUT4(tmp->mantissa);
  PUT4(tmp->exponent);
}

CodeInd MergeSubSequence(CodeInd younger, CodeInd older, Byte n)
{
  CodeInd c,d;
  Byte* code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  int trysize=INSTR_instrSize(try);
  int trustsize=INSTR_instrSize(trust);
  
  //Check plurality of the younger definition
  if(code[younger]==try||code[younger]==try_else)
  {
    //Younger is plural.
    c=younger;
    if(code[c]==try)
      c+=trysize;
    else //code[c]==try_else
      c=*(CodeInd*)(code+c+2*INSTR_LLen);
    
    //Get c to the trust instruction for the sequence.
    while(code[c]!=trust)
    {
      if(code[c]==retry)
      {
        c+=trysize;
      }
      else
      {
        ASSERT(code[c]==retry_else,"Unexpected subsequence instruction")
        c=*(CodeInd*)(code+c+2*INSTR_LLen);
      }
    }
    
    //Convert this into a retry else.
    code[c]=retry_else;
    //Move c to the address of the else label.
    c+=2*INSTR_LLen;
    
    //Check for plurality of the older subsequence.
    if(code[older]==try||code[older]==try_else)
    {
      //Older is plural.
      if(code[older]==try)
        code[older]=retry;
      else //code[older]==try_else
        code[older]=retry_else;
      //Fix label
      *(CodeInd*)(code+c)=older;
    }
    else
    {
      //Older is singular, so make a new block for the choice point info.
      d=LK_VECTOR_Grow(&Code,trustsize);
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
      *(CodeInd*)(code+c)=d;
      code[d]=trust;
      code[d+1]=n;
      *(CodeInd*)(code+d+INSTR_LLen)=older;
    }
    return younger;
  }
  else
  {
    //Younger is singular.
    if(code[older]==try||code[older]==try_else)
    {
      //Older is plural.
      if(code[older]==try)
        code[older]=retry;
      else //code[older]==try_else
        code[older]=retry_else;
      
      //Make a new try_else block
      c=LK_VECTOR_Grow(&Code,trustsize);
      code=LK_VECTOR_GetPtr(&Code,0);
      code[c]=try_else;
      code[c+1]=n;
      *(CodeInd*)(code+c+INSTR_LLen)=younger;
      *(CodeInd*)(code+c+2*INSTR_LLen)=older;
      return c;
    }
    else
    {
      //Both are singular, create a try->trust block
      c=LK_VECTOR_Grow(&Code,trysize+trustsize);
      code=LK_VECTOR_GetPtr(&Code,0);
      code[c]=try;
      code[c+1]=n;
      *(CodeInd*)(code+c+INSTR_LLen)=younger;
      code[c+trysize]=trust;
      code[c+trysize+1]=n;
      *(CodeInd*)(code+c+trysize+INSTR_LLen)=older;
      return c;
    }
  }
}

//Append definition at older to definition at younger.
CodeInd MergeDefs(CodeInd older, CodeInd younger)
{
  Byte* code=LK_VECTOR_GetPtr(&Code,0);
  CodeInd c=-1;
  int n=-1;
  
  //Assert switch_on_reg for younger.
  ASSERT(code[younger]==switch_on_reg,"Expected \"switch_on_reg\" at beginning of younger definition.")
  
  //Get predicate arity.
  //Set c to L1 of switch_on_reg.
  c=*(CodeInd*)(code+younger+sizeof(CodeInd));
  
  //Assert that the opcode at c is try.
  ASSERT(code[c]==try,"Expected \"try\" at branch L1 after switch_on_reg.")
  
  //Set n to the arity of the predicate.
  n=code[c+1];
  //Change the try -> trust_ext block to fails.
  code[c]=fail;
  code[c+sizeof(CodeInd)]=fail;
  code[c+2*sizeof(CodeInd)]=fail;
  
  //Get procedure address
  //Set c to L2 of switch_on_reg
  c=*(CodeInd*)(code+younger+2*sizeof(CodeInd));
  //Delete switch_on_reg
  code[younger]=fail;
  code[younger+sizeof(CodeInd)]=fail;
  code[younger+2*sizeof(CodeInd)]=fail;
  //Set c to the start address of the definition.
  younger=c;
  
  //Check for switch_on_reg for older
  if(code[older]==switch_on_reg)
  {
    //Set c to older's L2
    c=*(CodeInd*)(code+older+2*sizeof(CodeInd));
    //Call MergeSequence on the sequences
    younger=MergeSequence(younger,c,n);
    //Set older's L2
    code=LK_VECTOR_GetPtr(&Code,0);
    *(CodeInd*)(code+older+2*sizeof(CodeInd))=younger;
    //Get the address of the try for the older definition.
    c=*(CodeInd*)(code+older+sizeof(CodeInd));
    
    //Assert that the opcode at c is try.
    ASSERT(code[c]==try,"Expected \"try\" at branch L1 after switch_on_reg.")
    //Fix the try clause.
    *(CodeInd*)(code+c+sizeof(CodeInd))=younger;
    return older;
  }
  else
  {
    return MergeSequence(younger,older,n);
  }
}

//Append definition at older to definition at younger.
CodeInd MergeSequence(CodeInd younger, CodeInd older, Byte n)
{
  CodeInd c;
  Byte* code=LK_VECTOR_GetPtr(&Code,0);
  int tmesize=INSTR_instrSize(try_me_else)*INSTR_LLen;
  
  //Check if the younger definition is singular or plural.
  if(code[younger]==try_me_else)
  {
    //Younger is plural, set c to jump to last clause
    c=*(CodeInd*)(code+younger+INSTR_LLen);
    while(code[c]==retry_me_else)
      c=*(CodeInd*)(code+c+INSTR_LLen);
    ASSERT(code[c]==trust_me,"Expected trust at end of definition.");
    
    //Check plurality of older definition.
    if(code[older]==try_me_else)
    {
      //Older is Plural, prep c for continuation.
      code[c]=retry_me_else;
      //code[c+1]=n already.
      //Check for indexing on both endpoints
      if((code[c+tmesize]==switch_on_term)&&(code[older+tmesize]==switch_on_term))
      {
        //Bridge over the indexing of the older
        *(CodeInd*)(code+c+INSTR_LLen)=*(CodeInd*)(code+older+INSTR_LLen);
        MergeTerm(c+tmesize,older+tmesize,n);
        return younger;
      }
      else
      {
        //Append whole older definition.
        *(CodeInd*)(code+c+INSTR_LLen)=older;
        //Change first older clause to continuation
        code[older]=retry_me_else;
        return younger;
      }
    }
    else
    {
      //Older is singular, younger plural, c is the last clause.
      //Check for indexing on both endpoints
      if((code[c+tmesize]==switch_on_term)&&(code[older]==switch_on_term))
      {
        //c is still last, merge indexed.
        MergeTerm(c+tmesize,older,n);
        return younger;
      }
      else
      {
        //can't merge in singular, fix choice point instructions.
        older-=tmesize;
        code[older]=trust_me;
        code[older+1]=n;
        code[c]=retry_me_else;
        *(CodeInd*)(code+c+INSTR_LLen)=older;
        return younger;
      }
    }
  }
  else
  {
    //Younger is singular, check older
    if(code[older]==try_me_else)
    {
      //Older is plural, give younger a try_me_else.
      younger-=tmesize;
      code[younger]=try_me_else;
      code[younger+1]=n;
        
      //Check for double endpoint indexing.
      if((code[younger+tmesize]==switch_on_term)&&(code[older+tmesize]==switch_on_term))
      {
        //Bridge over first old clause.
        *(CodeInd*)(code+younger+INSTR_LLen)=*(CodeInd*)(code+older+INSTR_LLen);
        //Merge indexing.
        MergeTerm(younger+tmesize,older+tmesize,n);
        return younger;
      }
      else
      {
        //Append old definition whole
        *(CodeInd*)(code+younger+INSTR_LLen)=older;
        code[older]=retry_me_else;
        return younger;
      }
    }
    else
    {
      //Older is singular.  Check indexing.
      if((code[younger]==switch_on_term)&&(code[older]==switch_on_term))
      {
        //Combine into singular definition.
        MergeTerm(younger,older,n);
        return younger;
      }
      else
      {
        //Merge to plural.
        younger-=tmesize;
        older-=tmesize;
        code[younger]=try_me_else;
        code[younger+1]=n;
        *(CodeInd*)(code+younger+INSTR_LLen)=older;
        code[older]=trust_me;
        code[older+1]=n;
        return younger;
      }
    }
  }
}

void MergeTerm(CodeInd younger, CodeInd older,Byte n)
{
  Byte* code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  
  CodeInd c,d;//Holder variables for the younger and older labels, respectively.
  //Combine variable subsequences.
  younger+=INSTR_LLen;
  older+=INSTR_LLen;
  c=MergeSequence(*(CodeInd*)(code+younger),*(CodeInd*)(code+older),n);
  //Reload code location.
  code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  //Set V label
  *(CodeInd*)(code+younger)=c;
  
  //Combine constant subsequences.
  Byte tabsize;
  younger+=INSTR_LLen;
  older+=INSTR_LLen;
  c=*(CodeInd*)(code+younger);
  d=*(CodeInd*)(code+older);
  
  //Check if older subsequence exists.
  if(code[d]==switch_on_constant)
  {
    //Check if younger case exists.
    if(code[c]==switch_on_constant)
    {
      tabsize=MergeHashTabs(*(HashTabInd*)(code+c+2),*(HashTabInd*)(code+d+2),n);
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
      //Update table size.
      code[c+1]=tabsize;
    }
    else
    {
      //Use only the existing older case.
      *(CodeInd*)(code+younger)=d;
    }
  }
  
  //Combine List subsequences.
  younger+=INSTR_LLen;
  older+=INSTR_LLen;
  c=*(CodeInd*)(code+younger);
  d=*(CodeInd*)(code+older);
  //Check older case
  if(code[d]!=fail)
  {
    //Check younger case
    if(code[c]!=fail)
    {
      d=MergeSubSequence(c,d,n);
      //Reload code variable.
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
    }
    *(CodeInd*)(code+younger)=d;
  }
  
  //Combine Bvr subsequences
  younger+=INSTR_LLen;
  older+=INSTR_LLen;
  c=*(CodeInd*)(code+younger);
  d=*(CodeInd*)(code+older);
  if(code[d]==switch_on_bvar)
  {
    if(code[c]==switch_on_bvar)
    {
      MergeBvrTabs(*(BvrTabInd*)(code+c+3),*(BvrTabInd*)(code+d+3),n);
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
      //Use the larger value limit.
      if(*(TwoBytes*)(code+c+1)<*(TwoBytes*)(code+d+1))
        *(TwoBytes*)(code+c+1)=*(TwoBytes*)(code+d+1);
    }
    else
    {
      *(CodeInd*)(code+younger)=d;
    }
  }
}
