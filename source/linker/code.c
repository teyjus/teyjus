//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>
#include "../include/standardlib.h"
#include "../tables/instructions.h"
#include "code.h"
#include "module.h"
#include "importtab.h"
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "hashtab.h"
#include "bvrtab.h"
#include "linker_message.h"
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
  INSTR_InstrCategory instrCat;
  INSTR_OperandType* opType;

  j=1;
  argid=0;
  instrCat = INSTR_instrType(call);
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
  Byte* code;
  Byte opcode;
  ConstInd tmpIndex;
  INSTR_InstrCategory instrCat;
  INSTR_OperandType* opType;
  int argid=0;

  if(size<=0){
    debug("No code for current module.\n");
    return;
  }
  debug("Loading 0x%lx bytes of code at %lx to offset 0x%lx.\n",size,lseek(fd,0,SEEK_CUR),offset);
  code=(Byte*)LK_VECTOR_GetPtr(&Code,offset);
  debug("After get\n");
  
  opcode=-1;
  instrCat=INSTR_CAT_X;
  opType=NULL;
  
  for(i=0;i<size;)
  {
    j=i;
    opcode=code[j++]=LK_FILE_GET1(fd);
    debug("\t%lx:%lx:[%x]%s\t",lseek(fd,0,SEEK_CUR)-1,i,opcode,INSTR_instrName(opcode));
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
          debug("ID:%d\t",CMData->SegmentID);
          j+=sizeof(Byte);
          break;
        
        case INSTR_R:
        case INSTR_E:
        case INSTR_N:
        case INSTR_I1:
        case INSTR_CE:
          *(Byte*)(code+j)=LK_FILE_GET1(fd);
          debug("%d\t",*(Byte*)(code+j));
          j+=sizeof(Byte);
          break;

//         case INSTR_I2:
//           *(TwoBytes*)(code+j)=GET2();
//           j+=sizeof(TwoBytes);
//           break;
          
        case INSTR_C:
          *(TwoBytes*)(code+j)=PackConstInd(GetConstInd(fd,CMData));
          debug("C:%d\t",*(TwoBytes*)(code+j));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_K:
          *(TwoBytes*)(code+j)=PackKindInd(GetKindInd(fd,CMData));
          debug("K:%d\t",*(TwoBytes*)(code+j));
          j+=sizeof(TwoBytes);
          break;
          
        case INSTR_MT:
          *(ImportTabInd*)(code+j)=GetImportTabInd(fd,CMData);
          debug("IT:%d\t",*(ImportTabInd*)(code+j));
          j+=sizeof(ImportTabInd);
          break;
          
        case INSTR_IT:
          *(ImplGoalInd*)(code+j)=GetImplGoalInd(fd,CMData);
          debug("IG:%d\t",*(ImplGoalInd*)(code+j));
          j+=sizeof(ImplGoalInd);
          break;
          
        case INSTR_HT:
          *(HashTabInd*)(code+j)=GetHashTabInd(fd,CMData);
          debug("HT:%d\t",*(HashTabInd*)(code+j));
          j+=sizeof(HashTabInd);
          break;
          
        case INSTR_BVT:
          *(BvrTabInd*)(code+j)=GetBvrTabInd(fd,CMData);
          debug("BT:%d\t",*(BvrTabInd*)(code+j));
          j+=sizeof(BvrTabInd);
          break;
          
        case INSTR_S:
          *(StringInd*)(code+j)=GetStringInd(fd,CMData);
          debug("S:%d\t",*(StringInd*)(code+j));
          j+=sizeof(StringInd);
          break;
        
        case INSTR_L:
          *(CodeInd*)(code+j)=GetCodeInd(fd,CMData);
          debug("L:%d\t",*(CodeInd*)(code+j));
          j+=sizeof(CodeInd);
          break;
          
        case INSTR_I:
          *(INT4*)(code+j)=LK_FILE_GET4(fd);
          debug("I:%d\t",*(INT4*)(code+j));
          j+=sizeof(INT4);
          break;

        case INSTR_F:
          *(INT4*)(code+j)=GetFloat(fd);
          debug("F:%d\t",*(INT4*)(code+j));
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
    debug("\n");
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
  char* code=(char *)LK_VECTOR_GetPtr(&Code,0);
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  int argid=0;

  debug("Writing 0x%lx bytes of instructions at %lx.\n",size,lseek(fd,0,SEEK_CUR));
  
  for(i=0;i<size;)
  {
    j=i;
    
    opcode=code[j++];
    instrCat=INSTR_instrType(opcode);
    opType=INSTR_operandTypes(instrCat);
    argid=-1;
    LK_FILE_PUT1(fd,opcode);
    debug("\t%lx:%lx:[%x]%s ",lseek(fd,0,SEEK_CUR)-1,i,opcode,INSTR_instrName(opcode));
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
          debug("SEG %d ",*(Byte*)(code+j));
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
          debug("L(%lx) ",*(CodeInd*)(code+j));
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
          bad("\nUnknown Operand Type %d\n",opType[argid]);
          EM_THROW(LK_LinkError);
          break;     /* null instruction */
      }
    }
    while(opType[argid]!=INSTR_X);
    debug("\n");
    i+=INSTR_instrSize(opcode);
  }
}

void MakeCallName(CodeInd from, int arity, ConstInd to)
{
  Byte* tmp=(Byte *)LK_VECTOR_GetPtr(&Code,from);
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
  Byte* tmp=(Byte *)LK_VECTOR_GetPtr(&Code,from);
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

CodeInd GetSwitchOnRegL1(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid L1 in switch_on_reg")
  return tmp;
}

CodeInd GetSwitchOnRegL2(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid L2 in switch_on_reg")
  return tmp;
}

void SetSwitchOnRegL2(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd)) = value;
}

void DeleteSwitchOnReg(Byte* code,CodeInd inst_addr)
{
  code[inst_addr]=fail;
  code[inst_addr+sizeof(CodeInd)]=fail;
  code[inst_addr+2*sizeof(CodeInd)]=fail;
}

void DeleteTrustExt(Byte* code, CodeInd inst_addr)
{
  code[inst_addr]=fail;
}

CodeInd GetSwitchOnTermLV(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid LV in switch_on_term")
    return tmp;
}

void SetSwitchOnTermLV(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+sizeof(CodeInd)) = value;
}

CodeInd GetSwitchOnTermLC(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid LC in switch_on_term")
    return tmp;
}

void SetSwitchOnTermLC(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd)) = value;
}

CodeInd GetSwitchOnTermLL(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+3*sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid LL in switch_on_term")
    return tmp;
}

void SetSwitchOnTermLL(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+3*sizeof(CodeInd)) = value;
}

CodeInd GetSwitchOnTermLB(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+4*sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid LB in switch_on_term")
    return tmp;
}

void DeleteSwitchOnTerm(Byte* code,CodeInd inst_addr)
{
  code[inst_addr]=fail;
  code[inst_addr+sizeof(CodeInd)]=fail;
  code[inst_addr+2*sizeof(CodeInd)]=fail;
  code[inst_addr+3*sizeof(CodeInd)]=fail;
  code[inst_addr+4*sizeof(CodeInd)]=fail;
}

void SetSwitchOnTermLB(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+4*sizeof(CodeInd)) = value;
}

void SetTryL1(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+sizeof(CodeInd)) = value;
}

void DeleteTry(Byte* code,CodeInd inst_addr)
{
  code[inst_addr]=fail;
  code[inst_addr+sizeof(CodeInd)]=fail;
}

CodeInd GetTryMeElseL1(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid L1 in try_me_else")
    return tmp;
}

void SetTryMeElseL1(Byte* code,CodeInd inst_addr,CodeInd value)
{
  *(CodeInd*)(code+inst_addr+sizeof(CodeInd)) = value;
}

CodeInd GetTryElseL2(Byte* code,CodeInd inst_addr)
{
  CodeInd tmp = *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd));
  ASSERT(tmp<LK_VECTOR_Size(&Code),"Invalid L2 in try_else")
  return tmp;
}

void SetTryElseL2(Byte* code,CodeInd inst_addr, CodeInd value)
{
  *(CodeInd*)(code+inst_addr+2*sizeof(CodeInd)) = value;
}

void DeleteTryMeElse(Byte* code,CodeInd inst_addr)
{
  code[inst_addr]=fail;
  code[inst_addr+sizeof(CodeInd)]=fail;
}

void SetTrustL1(Byte* code,CodeInd inst_addr, CodeInd value)
{
  *(CodeInd*)(code+inst_addr+sizeof(CodeInd)) = value;
}

HashTabInd GetSwitchOnConstantHT(Byte* code, CodeInd inst_addr)
{
  return *(HashTabInd*)(code+inst_addr+sizeof(CodeInd));
}

void DeleteSwitchOnConstant(Byte* code,CodeInd inst_addr)
{
  code[inst_addr]=fail;
  code[inst_addr+sizeof(CodeInd)]=fail;
}

CodeInd MergeSubSequence(CodeInd younger, CodeInd older, Byte n)
{
  CodeInd c,d;
  Byte* code;
  int trysize=INSTR_instrSize(try);
  int trustsize=INSTR_instrSize(trust);

  debug("Merging subsequences %lx & %lx\n",younger,older);

  code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  
  //Check plurality of the younger definition
  if(code[younger]==try||code[younger]==try_else)
  {
    //Younger is plural.
    c=younger;
    if(code[c]==try)
      c+=trysize;
    else //code[c]==try_else
      c=GetTryElseL2(code,c);
    
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
        c=GetTryElseL2(code,c);
      }
    }
    
    //Convert this into a retry else.
    code[c]=retry_else;
    
    //Check for plurality of the older subsequence.
    if(code[older]==try||code[older]==try_else)
    {
      //Older is plural.
      if(code[older]==try)
        code[older]=retry;
      else //code[older]==try_else
        code[older]=retry_else;
      SetTryElseL2(code,c,older);
    }
    else
    {
      //Older is singular, so make a new block for the choice point info.
      d=LK_VECTOR_Grow(&Code,trustsize);
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
      SetTryElseL2(code,c,d);
      code[d]=trust;
      code[d+1]=n;
      SetTrustL1(code,d,older);
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
      code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
      code[c]=try_else;
      code[c+1]=n;
      *(CodeInd*)(code+c+INSTR_LLen)=younger;
      SetTryElseL2(code,c,older);
      return c;
    }
    else
    {
      //Both are singular, create a try->trust block
      c=LK_VECTOR_Grow(&Code,trysize+trustsize);
      code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
      code[c]=try;
      code[c+1]=n;
      SetTryL1(code,c,younger);
      code[c+trysize]=trust;
      code[c+trysize+1]=n;
      SetTrustL1(code,c+trysize,older);
      return c;
    }
  }
}

int TidySwitchOnReg(CodeInd* pyounger)
{
  Byte* code;
  CodeInd c=-1;
  int n=-1;
  CodeInd younger;
  
  debug("Tidying switch_on_reg at %lx\n",*pyounger);
  code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
  
  younger=*pyounger;
  
  if(code[younger]!=switch_on_reg)
    return -1;
  
  //Get predicate arity.
  c=GetSwitchOnRegL1(code,younger);
  ASSERT(code[c]==try,"Expected \"try\" at branch L1 after switch_on_reg.")
  
  //Set n to the arity of the predicate.
  n=code[c+1];
  DeleteTry(code,c);
  DeleteTrustExt(code,c+INSTR_instrSize(try));
  
  //Get procedure address
  c=GetSwitchOnRegL2(code,younger);
  DeleteSwitchOnReg(code,younger);
  //return the start address of the definition.
  debug("Setting pyounger %lx\n",c);
  *pyounger=c;
  return n;
}

//Append definition at older to definition at younger.
CodeInd MergeDefs(CodeInd older, CodeInd younger)
{
  Byte* code;
  int n;

  debug("Merging defs at %lx and %lx\n",older,younger);
  ASSERT(older<LK_VECTOR_Size(&Code),"Invalid argument to MergeDefs")
  ASSERT(younger<LK_VECTOR_Size(&Code),"Invalid argument to MergeDefs")
  code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
  ASSERT(code[younger]==switch_on_reg,"Expected \"switch_on_reg\" at beginning of younger definition.")
  
  n = TidySwitchOnReg(&younger);
  
  if(code[older]==switch_on_reg)
  {
    CodeInd c=GetSwitchOnRegL2(code,older);
    younger=MergeSequence(younger,c,n);
    
    code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
    SetSwitchOnRegL2(code,older,younger);
    
    c=GetSwitchOnRegL1(code,older);
    ASSERT(code[c]==try,"Expected \"try\" at branch L1 after switch_on_reg.")
    SetTryL1(code,c,younger);
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
  Byte* code;
  int tmesize;

  debug("Merging sequences %lx & %lx\n",younger,older);
  
  ASSERT(younger<LK_VECTOR_Size(&Code),"Invalid older in MergeSequence")
  ASSERT(older<LK_VECTOR_Size(&Code),"Invalid younger in MergeSequence")
  code=(Byte *)LK_VECTOR_GetPtr(&Code,0);
  tmesize=INSTR_instrSize(try_me_else);
  
  //Check if the younger definition is singular or plural.
  if(code[younger]==try_me_else)
  {
    //Younger is plural, set c to jump to last clause
    c=GetTryMeElseL1(code,younger);
    ASSERT(c<LK_VECTOR_Size(&Code),"Invalid L1 in try_me_else")
    while(code[c]==retry_me_else){
      c=GetTryMeElseL1(code,c);
      ASSERT(c<LK_VECTOR_Size(&Code),"Invalid L1 in retry_me_else")
    }
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
        CodeInd tmp = GetTryMeElseL1(code,older);
        SetTryMeElseL1(code,c,tmp);
        DeleteTryMeElse(code,older);
        MergeTerm(c+tmesize,older+tmesize,n);
        return younger;
      }
      else
      {
        //Append whole older definition.
        SetTryMeElseL1(code,c,older);
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
        SetTryMeElseL1(code,c,older);
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
        CodeInd tmp = GetTryMeElseL1(code,older);
        SetTryMeElseL1(code,younger,tmp);
        DeleteTryMeElse(code,older);
        MergeTerm(younger+tmesize,older+tmesize,n);
        return younger;
      }
      else
      {
        //Append old definition whole
        SetTryMeElseL1(code,younger,older);
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
        SetTryMeElseL1(code,younger,older);
        code[older]=trust_me;
        code[older+1]=n;
        return younger;
      }
    }
  }
}

void MergeTerm(CodeInd younger, CodeInd older,Byte n)
{
  Byte tabsize;
  Byte* code;
  CodeInd c,d;//Holder variables for the younger and older labels, respectively.

  debug("Merging terms %lx & %lx\n",younger,older);
  code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  
  //Combine variable subsequences.
  
  c=GetSwitchOnTermLV(code,younger);
  d=GetSwitchOnTermLV(code,older);
  d=MergeSequence(c,d,n);
  //Reload code location.
  code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
  SetSwitchOnTermLV(code,younger,d);
  
  //Combine constant subsequences.
  c=GetSwitchOnTermLC(code,younger);
  d=GetSwitchOnTermLC(code,older);
  
  //Check if older subsequence exists.
  if(code[d]==switch_on_constant)
  {
    //Check if younger case exists.
    if(code[c]==switch_on_constant)
    {
      HashTabInd youngHT = GetSwitchOnConstantHT(code,c);
      HashTabInd oldHT = GetSwitchOnConstantHT(code,d);
      DeleteSwitchOnConstant(code,d);
      tabsize=MergeHashTabs(youngHT,oldHT,n);
      code=(Byte*)LK_VECTOR_GetPtr(&Code,0);
      //Update table size.
      code[c+1]=tabsize;
    }
    else
    {
      //Use only the existing older case.
      SetSwitchOnTermLC(code,younger,d);
    }
  }
  
  //Combine List subsequences.
  c=GetSwitchOnTermLL(code,younger);
  d=GetSwitchOnTermLL(code,older);
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
    SetSwitchOnTermLL(code,younger,d);
  }
  
  //Combine Bvr subsequences
  c=GetSwitchOnTermLB(code,younger);
  d=GetSwitchOnTermLB(code,older);
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
      SetSwitchOnTermLB(code,younger,d);
    }
  }
  DeleteSwitchOnTerm(code,older);
}
