
#include "loader.h"
#include "implgoal.c"
#include "code.h"
#include "../simulator/abstmachine.h"



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

 Can the following instructions appear in compiled queries?
 INSTR_MT -> import tables
   -- no?
 INSTR_IT -> implication tables
   -- yes
 INSTR_HT -> hash tables
   -- no?
 INSTR_BVT -> bound variable tables
   -- yes?
 INSTR_S -> string table
   -- yes?

My guess is that the only required information is:
 * (TwoBytes) Number of local consts
 * (TwoBytes) Number of global consts
 * (TwoBytes) Number of global kinds


*/


// Load a compiled query code and its implication table into the heap
void LD_LOADQ_LoadCompiledQuery(char* modname)
{
  // set up a virtual GMT module
  MEM_GmtEnt* ent;

  ent->modSpaceBeg = (WordPtr)AM_hreg;
  ent->modSpaceEnd = ent->modSpaceBegin;

  ent->codeSpaceEnd = (WordPtr)AM_heapEnd;
  // This will set LD_CODE_codeSpaceBeg
  LD_CODE_LoadCodeSize(ent);

  // This depends on the following functions:
  //  - LD_CODE_GetCodeInd
  //  - LD_CONST_GetConstInd
  /* Read implication table */
  LD_IMPLGOAL_LoadImplGoals(ent);

  /* A query is relative to a module entry. 
   * Given a loaded entry we must recover the functions:
   *  - GetConstInd(ent,const)
   *  - GetKindInd(ent,kind)
   *  - GetImportTabAddr(ent,...)
   *  - GetImplGoalAddr(ent,...)
   *  - GetHashTabAddr(ent,...)
   *  - GetBvrTabAddr(ent,...)
   *  - GetStringAddr(ent,...)
   *  - GetCodeInd(ent,...)

   * and write my own 
   *   LoadQueryCode(...)
   */

  // This assumes we have assigned:
  //  - LD_CONST_numGConsts
  //  - LD_CONST_numLConsts
  //  - LD_KIND_numGKinds
  //  - LD_IMPORTTAB_numImportTabs
  //  - LD_IMPORTTAB_ImportTabs
  //  - LD_IMPLGOAL_numImplGoals
  //  - LD_IMPLGOAL_ImplGoals
  //  - LD_HASHTAB_numHashTabs
  //  - LD_HASHTAB_HashTabs
  //  - LD_BVRTAB_numBvrTabs
  //  - LD_BVRTAB_BvrTabs
  //  - LD_STRING_numStrings
  //  - LD_STRING_Strings
  /* Read bytecode */
  LD_CODE_LoadCode(codesize, code_ptr);

  // Shrink the bottom of the heap
  AM_heapEnd = ent->codeSpaceBeg;

  // open the file [module].tmp
  LD_FILE_Open(LD_LOADER_makePath(modname),QUERY_EXT);



  AM_hreg = ent->codeSpaceEnd;

  return;
}
