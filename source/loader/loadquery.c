#include <stdio.h>
#include <string.h>

#include "ld_message.h"
#include "file.h"
#include "loader.h"
#include "hashtab.h"
#include "strings.h"
#include "const.h"
#include "implgoal.h"
#include "code.h"
#include "../simulator/abstmachine.h"

#define QUERY_EXT ".lpq"

/* Load the bytecode for a query into the heap.
 * We only care about the bytecode and the implication table.
 * Registers we care about:
 *  AM_hreg  --> simulator/abstmachine heap register
 *  AM_hbreg --> heap backtrack point (probably needs to be initialized to AM_hreg)
 *  AM_preg  --> simulator/abstmachine program entrypoint

 See implnotes/bytecode.txt for how bytecode files are formated

 We for the loaded toplevel module, will have to write into 
 the bytecode file the following constants:
 * Code size
 * (TwoBytes) Number of local consts
 * (TwoBytes) Number of global consts
 * (TwoBytes) Number of global kinds
 * (TwoBytes) Number of import tables
 * (WordPtr*) import tables  (points to module space in GMT entry)
 * (TwoBytes) Number of implication goals
 * (WordPtr*) Implication goals (points to module space in GMT entry)
 * (TwoBytes) Number of hash tables
 * (WordPtr*) Hash tables (points to module space in GMT entry)
 * (TwoBytes) Number of bound variable tables
 * (WordPtr*) Bound variable tables (points to module space in GMT entry)
 * (TwoBytes) Number of strings
 * (DF_StrDataPtr*) strings (points to module space in GMT entry)


 The problematic entries are the memory pointers to the module space in the GMT entry,
 which we do not know at compile time and are discarded after loading. 
 However it is possible that we don't require all of them.

 The following opcodes may appear in compiled queries:
 * INSTR_IT -> implication tables
 * INSTR_HT -> hash tables
 * INSTR_BVT -> bound variable tables
 * INSTR_S -> string table
but the following cannot:
* INSTR_MT -> import tables

My guess is that the only required information is:
 * (TwoBytes) Number of local consts
 * (TwoBytes) Number of global consts
 * (TwoBytes) Number of global kinds


*/

// Load a compiled query code and its implication table into the heap
void LD_LOADQ_LoadCompiledQuery(MEM_GmtEnt* parent_ent,
								int num_typeskels, int num_consts,
								char* modName)
{
  printf("loading compiled query - %s\n", modName);
	
  // set up a virtual GMT module
  MEM_GmtEnt ent;

  printf("HeapBeg: %x\n", (unsigned long)AM_heapBeg);
  printf("MemTop:  %x\n", (unsigned long)MEM_memTop);
  printf("Hreg:    %x\n", (unsigned long)AM_hreg);
  printf("HeapEnd: %x\n", (unsigned long)AM_heapEnd);
  printf("MemBot:  %x\n", (unsigned long)MEM_memBot);

  /* We have to use AM_heapEnd rather than MEM_memBot
   * because space has already been fixed for the heap
   */
  ent.codeSpaceEnd = AM_heapEnd;

  /* We have to use AM_hreg rather than MEM_memTop
   * because we have already layed out input registers
   * onto the heap */
  ent.modSpaceEnd = AM_hreg;

  printf("loading file\n");
  EM_TRY{
	LD_FILE_Open(LD_LOADER_makePath(modName),QUERY_EXT);
	
	/* This will set LD_CODE_codeSpaceBeg */
	printf("loading code size\n");
	LD_CODE_LoadCodeSize(&ent);
  }EM_CATCH{
	EM_THROW(LD_LoadError);
  }


  // for now we assume no hidden constants + type skeletons!
    
  // Load the hidden constant table and local kind table
  // in the contiguous CST module space of the loaded module
  /* ent.modSpaceEnd = parent_ent->kstBase + (num_typeskels * MEM_TST_ENTRY_SIZE); */
  /* LD_TYSKEL_LoadTst(&ent); */
  
  /* ent.modSpaceEnd = parent_ent->cstBase + (num_consts * MEM_CST_ENTRY_SIZE); */
  /* LD_CONST_LoadCst(&ent); */

  ent.modSpaceBeg = ent.modSpaceEnd = AM_hreg; //MEM_memTop;
 
  /* Read string tables */
  printf("loading string table\n");
  LD_STRING_LoadStrings(&ent);

  // Read implication tables
  printf("loading implication tables\n");
  LD_IMPLGOAL_LoadImplGoals(&ent, 1);

  /* Read hash tables */
  printf("loading hashtables\n");
  LD_HASHTAB_LoadHashTabs(&ent, 1);

  /* Read bound variable tables */
  printf("loading bvr tables\n");
  LD_BVRTAB_LoadBvrTabs(&ent);
  
  /* Read bytecode in query mode */
  printf("loading code\n");
  LD_CODE_LoadCode(&ent,1);

  // TODO: We need to overwrite code space for the next query!
  // Module space is reset automatically, but not code space!
  AM_hreg = ent.modSpaceEnd;


  
  // Shrink the bottom of the heap to prevent overwrite,
  // TODO: But we need to be able to overwrite in future queries
  AM_heapEnd=(MemPtr)ent.codeSpaceBeg;

  printf("CodeSpaceBeg: %x\n", ent.codeSpaceBeg);
  printf("CodeSpaceEnd: %x\n", ent.codeSpaceEnd);


  // TODO: Very dirty hack:
  AM_preg = ent.codeSpaceBeg;
  
  
  /* printf("HeapBeg: %lu\n", (unsigned long)AM_heapBeg); */
  /* printf("Hreg: %lu\n", (unsigned long)AM_hreg); */
  /* printf("HeapEnd: %lu\n", (WordPtr)AM_heapEnd); */
}
