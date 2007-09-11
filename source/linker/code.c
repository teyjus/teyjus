#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "../tables/instructions.h"
#include "code.h"
#include "module.h"
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "hashtab.h"
#include "bvrtab.h"
#include "message.h"
#include "const.h"
#include "kind.h"
#include "../system/error.h"

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

CodeInd MergeSequence(CodeInd younger, CodeInd older, Byte n);
void MergeTerm(CodeInd younger, CodeInd older,Byte n);

#define EXECUTE 0
#define CALL 1
#define ADDR 0
#define NAME 1
#define LABEL 0
#define ARITY 1
int callOffsets[2][2][2];

void InitOffsets()
{
  int j;
  int argid;
  j=1;
  argid=0;
  INSTR_InstrCategory instrCat = INSTR_instrType(call);
  INSTR_OperandType* opType = INSTR_operandTypes(instrCat);
  do{
    switch(opType[argid])
    {
      case INSTR_P:
        j++;
        break;
          
      case INSTR_WP:
        j+=sizeof(Word);
        break;
          
      case INSTR_I1:
        callOffsets[CALL][ADDR][ARITY]=j++;
        break;

      case INSTR_L:
        callOffsets[CALL][ADDR][LABEL]=j;
        j+=sizeof(CodeInd);
        break;
      default:
        bad("Internal Error: in code.c InitOffsets():A\n");
        EM_THROW(LK_LinkError);
    }
    argid++;
  }while(opType[argid]!=INSTR_X);
  
  j=1;
  argid=0;
  instrCat = INSTR_instrType(execute);
  opType = INSTR_operandTypes(instrCat);
  do{
    switch(opType[argid])
    {
      case INSTR_P:
        j++;
        break;
          
      case INSTR_WP:
        j+=sizeof(Word);
        break;
 
      case INSTR_L:
        callOffsets[EXECUTE][ADDR][LABEL]=j;
        j+=sizeof(CodeInd);
        break;
      default:
        bad("Internal Error: in code.c InitOffsets():B\n");
        EM_THROW(LK_LinkError);
    }
    argid++;
  }while(opType[argid]!=INSTR_X);
  
  j=1;
  argid=0;
  instrCat = INSTR_instrType(call_name);
  opType = INSTR_operandTypes(instrCat);
  do{
    switch(opType[argid])
    {
      case INSTR_P:
        j++;
        break;
          
      case INSTR_WP:
        j+=sizeof(Word);
        break;
          
      case INSTR_I1:
        callOffsets[CALL][NAME][ARITY]=j++;
        break;
        
      case INSTR_C:
        callOffsets[CALL][NAME][LABEL]=j;
        j+=sizeof(ConstInd);
        break;
      default:
        bad("Internal Error: in code.c InitOffsets():C %x\n",opType[argid]);
        EM_THROW(LK_LinkError);
    }
    argid++;
  }while(opType[argid]!=INSTR_X);
  
  j=1;
  argid=0;
  instrCat = INSTR_instrType(execute_name);
  opType = INSTR_operandTypes(instrCat);
  do{
    switch(opType[argid])
    {
      case INSTR_P:
        j++;
        break;
          
      case INSTR_WP:
        j+=sizeof(Word);
        break;
          
      case INSTR_C:
        callOffsets[EXECUTE][NAME][LABEL]=j;
        j+=sizeof(ConstInd);
        break;
      default:
        bad("Internal Error: in code.c InitOffsets():D\n");
        EM_THROW(LK_LinkError);
    }
    argid++;
  }while(opType[argid]!=INSTR_X);
}

void InitTCode()
{
  LK_VECTOR_Init(&Code,1024,sizeof(char));
  LK_VECTOR_Init(&Floats,32,sizeof(struct BCFloat));
  InitOffsets();
}

INT4 GetFloat(int fd)
{
  int i=LK_VECTOR_Grow(&Floats,1);
  struct BCFloat* tmp=(struct BCFloat*)LK_VECTOR_GetPtr(&Floats,i);
  tmp->mantissa=LK_FILE_GET4(fd);
  tmp->exponent=LK_FILE_GET4(fd);
  return i;
}

void PutFloat(int fd, INT4 i)
{
  struct BCFloat* tmp=(struct BCFloat*)LK_VECTOR_GetPtr(&Floats,i);
  LK_FILE_PUT4(fd,tmp->mantissa);
  LK_FILE_PUT4(fd,tmp->exponent);
}

void LoadCode(int fd, struct Module_st* CMData)
{
  Word i,j;
  Word offset=CMData->CodeOffset;
  Word size=CMData->CodeSize;
  
  debug("Loading 0x%lx bytes of code at %lx to offset 0x%lx.\n",size,lseek(fd,0,SEEK_CUR),offset);
  Byte* code=(Byte*)LK_VECTOR_GetPtr(&Code,offset);
  ConstInd tmpIndex;
  
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  
  int argid=0;
  for(i=0;i<size;)
  {
    j=i;
    opcode=code[j++]=LK_FILE_GET1(fd);
    debug("\t%lx:%lx:[%x]%s\n",lseek(fd,0,SEEK_CUR)-1,i,opcode,INSTR_instrName(opcode));
    if(opcode==call_link_only)
    {
      code[j]=LK_FILE_GET1(fd);
      tmpIndex=GetConstInd(fd,CMData);
      PushCall(CMData->Pit,tmpIndex,offset+i,code[j]);
      i += INSTR_instrSize(opcode);
      
      continue;
    }
    else if(opcode==execute_link_only)
    {
      tmpIndex=GetConstInd(fd,CMData);
      PushCall(CMData->Pit,tmpIndex,offset+i,-1);
      i += INSTR_instrSize(opcode);
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
          
        case INSTR_WP:
          j+=sizeof(Word);
          break;
          
        case INSTR_SEG:
          LK_FILE_GET1(fd);//Consume dummy argument
          *(Byte*)(code+j)=CMData->SegmentID;
          j+=sizeof(Byte);
          break;
        
        case INSTR_R:
        case INSTR_E:
        case INSTR_N:
        case INSTR_I1:
        case INSTR_CE:
          *(Byte*)(code+j)=LK_FILE_GET1(fd);
          j+=sizeof(Byte);
          break;

//         case INSTR_I2:
//           *(TwoBytes*)(code+j)=GET2();
//           j+=sizeof(TwoBytes);
//           break;
          
        case INSTR_C:
          *(TwoBytes*)(code+j)=PackConstInd(GetConstInd(fd,CMData));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_K:
          *(TwoBytes*)(code+j)=PackKindInd(GetKindInd(fd,CMData));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_MT:
          *(ImportTabInd*)(code+j)=GetImportTabInd(fd,CMData);
          j+=sizeof(ImportTabInd);
          break;
          
        case INSTR_IT:
          *(ImplGoalInd*)(code+j)=GetImplGoalInd(fd,CMData);
          j+=sizeof(ImplGoalInd);
          break;
          
        case INSTR_HT:
          *(HashTabInd*)(code+j)=GetHashTabInd(fd,CMData);
          j+=sizeof(HashTabInd);
          break;
          
        case INSTR_BVT:
          *(BvrTabInd*)(code+j)=GetBvrTabInd(fd,CMData);
          j+=sizeof(BvrTabInd);
          break;
          
        case INSTR_S:
          *(StringInd*)(code+j)=GetStringInd(fd,CMData);
          j+=sizeof(StringInd);
          break;
        
        case INSTR_L:
          *(CodeInd*)(code+j)=GetCodeInd(fd,CMData);
          j+=sizeof(CodeInd);
          break;
          
        case INSTR_I:
          *(INT4*)(code+j)=LK_FILE_GET4(fd);
          j+=sizeof(INT4);
          break;

        case INSTR_F:
          *(INT4*)(code+j)=GetFloat(fd);
          j+=sizeof(INT4);
          break;
          
        case INSTR_X:
          break;
        default:
          bad("Unknown Operand Type %d\n",opType[argid]);
          EM_THROW(LK_LinkError);
          break;
      }
      argid++;
    }
    while(opType[argid]!=INSTR_X);
    i += INSTR_instrSize(opcode);
  }
  debug("NOWD -> %d[%d,%d]\n",166,(*(ConstInd*)(code+166)).gl_flag,(*(ConstInd*)(code+166)).index);
          
}

void LoadCodeSize(int fd, struct Module_st* CMData)
{
  int size=CMData->CodeSize=(int)LK_FILE_GETWord(fd);
  CMData->CodeOffset=LK_VECTOR_Grow(&Code,size);
}

void FreeCode()
{
  LK_VECTOR_Free(&Code);
}

void WriteCodeSize(int fd)
{
  debug("Writing code size 0x%lx at %lx.\n",LK_VECTOR_Size(&Code),lseek(fd,0,SEEK_CUR));
  LK_FILE_PUTWord(fd,(Word)LK_VECTOR_Size(&Code));
}

void WriteCode(int fd)
{
  Word i,j;
  Word size=LK_VECTOR_Size(&Code);
  char* code=LK_VECTOR_GetPtr(&Code,0);
  
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  debug("Writing 0x%lx bytes of instructions at %lx.\n",size,lseek(fd,0,SEEK_CUR));
  int argid=0;
  for(i=0;i<size;)
  {
    j=i;
    
    opcode=code[j++];
    instrCat=INSTR_instrType(opcode);
    opType=INSTR_operandTypes(instrCat);
    argid=-1;
    LK_FILE_PUT1(fd,opcode);
    debug("\t%lx:%lx:[%x]%s\n",lseek(fd,0,SEEK_CUR)-1,i,opcode,INSTR_instrName(opcode));
    do{
      argid++;
      switch(opType[argid])
      {
        case INSTR_P:
          j++;
          break;
          
        case INSTR_WP:
          j+=sizeof(Word);
          break;
        
        case INSTR_SEG:
          debug("Putting segment %d\n",*(Byte*)(code+j));
        case INSTR_R:
        case INSTR_E:
        case INSTR_N:
        case INSTR_I1:
        case INSTR_CE:
          LK_FILE_PUT1(fd,*(Byte*)(code+j));
          j+=sizeof(Byte);
          break;

//         case INSTR_I2:
//           PUT2(*(TwoBytes*)(code+j));
//           j+=sizeof(TwoBytes);
//           break;
          
        case INSTR_C:
          PutConstInd(fd,UnPackConstInd(*(TwoBytes*)(code+j)));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_K:
          PutKindInd(fd,UnPackKindInd(*(TwoBytes*)(code+j)));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_MT:
          PutImportTabInd(fd,*(ImportTabInd*)(code+j));
          j+=sizeof(ImportTabInd);
          break;
          
        case INSTR_IT:
          PutImplGoalInd(fd,*(ImplGoalInd*)(code+j));
          j+=sizeof(ImplGoalInd);
          break;
          
        case INSTR_HT:
          PutHashTabInd(fd,*(HashTabInd*)(code+j));
          j+=sizeof(HashTabInd);
          break;
          
        case INSTR_BVT:
          PutBvrTabInd(fd,*(BvrTabInd*)(code+j));
          j+=sizeof(BvrTabInd);
          break;
          
        case INSTR_S:
          PutStringInd(fd,*(StringInd*)(code+j));
          j+=sizeof(StringInd);
          break;
        
        case INSTR_L:
          PutCodeInd(fd,*(CodeInd*)(code+j));
          j+=sizeof(CodeInd);
          break;
          
        case INSTR_I:
          LK_FILE_PUT4(fd,*(INT4*)(code+j));
          j+=sizeof(INT4);
          break;

        case INSTR_F:
          PutFloat(fd,*(INT4*)(code+j));
          j+=sizeof(INT4);
          break;
          
        case INSTR_X:
          break;
        default:
          bad("Unknown Operand Type %d\n",opType[argid]);
          EM_THROW(LK_LinkError);
          break;     /* null instruction */
      }
    }
    while(opType[argid]!=INSTR_X);
    
    i+=INSTR_instrSize(opcode);
  }
}

void MakeCallName(CodeInd from, int arity, ConstInd to)
{
  Byte* tmp=LK_VECTOR_GetPtr(&Code,from);
  if(-1==arity)
  {
    tmp[0]=execute_name;
    *(TwoBytes*)(tmp+callOffsets[EXECUTE][NAME][LABEL])=PackConstInd(to);
  }
  else
  {
    tmp[0]=call_name;
    tmp[callOffsets[CALL][NAME][ARITY]]=arity;
    *(TwoBytes*)(tmp+callOffsets[CALL][NAME][LABEL])=PackConstInd(to);
  }
}
  
void MakeCall(CodeInd from, int arity, CodeInd to)
{
  Byte* tmp=LK_VECTOR_GetPtr(&Code,from);
  if(-1==arity)
  {
    tmp[0]=execute;
    *(CodeInd*)(tmp+callOffsets[EXECUTE][ADDR][LABEL])=to;
  }
  else
  {
    tmp[0]=call;
    tmp[callOffsets[CALL][ADDR][ARITY]]=arity;
    *(CodeInd*)(tmp+callOffsets[CALL][ADDR][LABEL])=to;
  }
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
  int tmesize=INSTR_instrSize(try_me_else);
  
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
