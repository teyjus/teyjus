#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "const.h"
#include "hashtab.h"
#include "bvrtab.h"
#include "implgoal.h"
#include "strings.h"
#include "../tables/instructions.h"
#include "code.h"
#include "importtab.h"
#include "ld_message.h"

WordPtr codeSpaceBeg;

int LD_CODE_LoadCode(MEM_GmtEnt* ent)
{
  int i,j;
  int size=ent->codeSpaceEnd - ent->codeSpaceBeg;
  Byte* code=(Byte*)(ent->codeSpaceBeg);
  
  LD_detail("Loading %d bytes of instructions\n",size);
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  
  int argid=0;
  for(i=0;i<size;)
  {
    j=i;
    opcode=code[j++]=LD_FILE_GET1();
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
        *(Byte*)(code+j)=LD_FILE_GET1();
        j+=sizeof(Byte);
        break;
        
        /*case INSTR_I2:
          *(TwoBytes*)(code+j)=LD_FILE_GET2();
          j+=sizeof(TwoBytes);
          break;
        */  
      case INSTR_C:///\todo Check on constant index size in code
        *(TwoBytes*)(code+j)=LD_CONST_GetConstInd();
        j+=sizeof(TwoBytes);
        break;
        
      case INSTR_K:
        *(TwoBytes*)(code+j)=LD_KIND_GetKindInd();
        j+=sizeof(TwoBytes);
        break;
        
      case INSTR_MT:///\todo Make sure table indexes are supposed to resolve to real addresses.
        *(WordPtr*)(code+j)=LD_IMPORTTAB_GetImportTabAddr();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_IT:
        *(WordPtr*)(code+j)=LD_IMPLGOAL_GetImplGoalAddr();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_HT:
        *(WordPtr*)(code+j)=LD_HASHTAB_GetHashTabAddr();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_BVT:
        *(WordPtr*)(code+j)=LD_BVRTAB_GetBvrTabAddr();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_S:
        *(DF_StrDataPtr*)(code+j)=LD_STRING_GetStringAddr();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_L:
        *(CSpacePtr*)(code+j)=LD_CODE_GetCodeInd();
        j+=sizeof(WordPtr);
        break;
        
      case INSTR_I:
        *(Word*)(code+j)=LD_FILE_GETWORD();
        j+=sizeof(Word);///\todo Check length of integer encoding across arch types
        break;
        
      case INSTR_F:
        *(Word*)(code+j)=LD_FILE_GETWORD();
        j+=sizeof(Word);///\todo Check length of float encoding across arch types
        break;
        
      default:
      case INSTR_X:
        break;
      }
      argid++;
    }
    while(opType[argid]!=INSTR_X);
    i+=INSTR_instrSize(opcode)*sizeof(Word);
  }
  return 0;
}


CSpacePtr LD_CODE_GetCodeInd()
{
  return codeSpaceBeg+(int)LD_FILE_GETWORD();
}

void LD_CODE_LoadCodeSize(MEM_GmtEnt* ent)
{
  Word codesize=LD_FILE_GETWORD();
  ent->codeSpaceBeg=ent->codeSpaceEnd-(int)codesize;
}
