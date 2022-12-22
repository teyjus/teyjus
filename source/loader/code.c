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
#include "file.h"
#include "../system/memory.h"
#include "loader.h"
#include "const.h"
#include "kind.h"
#include "hashtab.h"
#include "bvrtab.h"
#include "implgoal.h"
#include "strings.h"
#include "../tables/instructions.h"
#include "code.h"
#include "importtab.h"
#include "ld_message.h"

CSpacePtr codeSpaceBeg;

INSTR_Float LD_getFloat()
{
  double mantissa = (double)LD_FILE_GET4();
  int    exponent = LD_FILE_GET4();
  int    exp31; 
  double significant = frexp(mantissa, &exp31);

  return ldexp(significant, exponent);
}

int LD_CODE_LoadCode(MEM_GmtEnt* ent, int query)
{
  int i,j;
  int size = (BytePtr)(ent->codeSpaceEnd) - (BytePtr)(ent->codeSpaceBeg) + 1;
  CSpacePtr code=(CSpacePtr)(ent->codeSpaceBeg);
  
  Byte opcode=-1;
  INSTR_InstrCategory instrCat=INSTR_CAT_X;
  INSTR_OperandType* opType=NULL;
  int argid=0;
  
  LD_detail("Loading %d bytes of instructions\n",size);

  
  for(i=0;i<size;) {
      j=i;
      opcode=code[j++]=LD_FILE_GET1();
      //fprintf(stderr, "opcode: %d, opcode address: %u\n", opcode, &(code[i]));
      LD_debug("%x:%s - ",j-1,INSTR_instrName(opcode));
      instrCat=INSTR_instrType(opcode);
      opType=INSTR_operandTypes(instrCat);
      argid=0;
      do{
          switch(opType[argid]) {
          case INSTR_P:
              j++;
              break;
              
          case INSTR_WP:
              j+=sizeof(Word);
              break;
          
          case INSTR_SEG:
          case INSTR_R:
          case INSTR_E:
          case INSTR_N:
          case INSTR_I1:
          case INSTR_CE:
              *(Byte*)(code+j)=LD_FILE_GET1();
              LD_debug("#%d ",*(Byte*)(code+j));
              j+=sizeof(Byte);
              break;

          case INSTR_C:///\todo Check on constant index size in code
			*(TwoBytes*)(code+j)=LD_CONST_GetConstIndQuery(query);
			j+=sizeof(TwoBytes);
			break;
              
          case INSTR_K:
			*(TwoBytes*)(code+j)=LD_KIND_GetKindIndQuery(query);
			j+=sizeof(TwoBytes);
			break;
              
          case INSTR_MT:///\todo Make sure table indexes are supposed to resolve to real addresses.
			// Queries should not have import table instructions
			if(query){
			  LD_error("Unexpected instruction `INSTR_MT` in query.");
			  EM_THROW(LD_LoadError);
			}
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
              LD_debug("L%x ",*(CSpacePtr*)(code+j)-codeSpaceBeg);
              j+=sizeof(WordPtr);
              break;
              
          case INSTR_I:
              *(INSTR_Int*)(code+j)=LD_FILE_GET4();
	      LD_debug("%d ",*(INSTR_Int*)(code+j));
              j+=sizeof(Word);///\todo Check length of integer encoding across arch types
              break;
        
          case INSTR_F:
    	      *(INSTR_Float*)(code+j)=LD_getFloat();
	      LD_debug("float %f\n", *(INSTR_Float*)(code+j));
              //*(Word*)(code+j)=LD_FILE_GETWORD();
              j+=sizeof(Word);///\todo Check length of float encoding across arch types
              break;
              
          case INSTR_X:
              break;
          default:
              LD_error("Unknown Operand Type %d\n",opType[argid]);
              EM_THROW(LD_LoadError);
              break;
          }
          argid++;
      }
      while(opType[argid]!=INSTR_X);
      //i+=INSTR_instrSize(opcode)*sizeof(Word);
      LD_debug("\n");
      i += INSTR_instrSize(opcode);
  }
  return 0;
}


CSpacePtr LD_CODE_GetCodeInd()
{
  return codeSpaceBeg+(long)LD_FILE_GETWORD();
}

/* allocate given size of BYTES at the bottom of system memory as the code */
/* space, and record the beginning and ending addresses into the module    */
/* table entry. -- XQ                                                      */
void LD_CODE_LoadCodeSize(MEM_GmtEnt* ent)
{
    long codesize = (long)LD_FILE_GETWORD();
	codeSpaceBeg  = ((CSpacePtr)(ent -> codeSpaceEnd)) - codesize + 1;
	ent->codeSpaceBeg = (WordPtr)codeSpaceBeg;

	if(ent->modSpaceEnd>ent->codeSpaceBeg){
	  LD_error("Out of module space.\n");
	  EM_THROW(LD_LoadError);
	}
}
