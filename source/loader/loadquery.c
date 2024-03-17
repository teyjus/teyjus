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

/* Load the bytecode for a compiled query into the heap.
   The only information we care about are:
   - Code size
   - instructions:
       stored at start of the heap (but loaded last)
   - type skeletons (may arise from hidden variables in the query):
       these are appended to the currently loaded module's type skel table,
	   since type skeleton indices are relative 
	   (Note: only addresses to the type skeletons are stored in the type skeleton
	   table, the actual type skeletons are stored in the heap)
   - hidden constants:
       these are appended to the currently loaded module's constant table,
	   since constant indices are relative
   - string table:
       stored at start of heap (addresses are absolute)
   - implication goal tables:
       stored at start of heap (addresses are absolute)
   - hash tables:
       stored at start of heap (addresses are absolute)
   - bound variable tables:
       stored at start of heap (addresses are absolute)

   See compiler/spitcode.ml for details.
   See implnotes/bytecode.txt and implnotes/linker/linkcode.txt.
   for how bytecode files are formated.
   All this data is overwritten by the next query.

   Registers we care about:
    AM_hreg  --> 
    1. input/output variables must be written onto the heap (see front/readterm.ml)
	2. code is loaded onto the heap
	2. tables are loaded onto the heap
    AM_preg  -->  in order to set the program entrypoint

 Note: 
 When a query is read, symbol tables for a module are loaded by loader/loadmodtab.ml
 before compilation takes place. The effect of this is that
 indices for kinds and constants (except for hidden constants) are independent 
 of the type of the constant (ie. global/local/hidden/pervasive)
 thus we must pass to the loading components whether or not we are loading a query.
 See loader/const.c:LD_CONST_GetConstIndQuery and
 loader/kind.c:LD_KIND_GetKindIndQuery.
*/

MemPtr LD_LOADQ_heapEnd = NULL;

// Assumes a pipe containing query code is already open
void LD_LOADQ_LoadCompiledQuery()
{
  // LD_verbosity = 3;
  // set up a virtual GMT module
  MEM_GmtEnt ent;

  // reset the end of the heap to overwrite
  // code from the previous query.
  if(LD_LOADQ_heapEnd){
	AM_heapEnd = LD_LOADQ_heapEnd;
  }
  /* printf("Heap End: %x\n", AM_heapEnd); */
  
  // set code space at the end of the heap
  ent.codeSpaceEnd = (WordPtr)AM_heapEnd;
  ent.modSpaceEnd = ent.modSpaceBeg = (WordPtr)AM_hreg;

  // These parameters are required to load the
  // type skeleton and constant tables
  ent.tstSize = MEM_currentModule -> tstSize;
  ent.cstSize = MEM_currentModule -> cstSize;
  ent.tstBase = MEM_currentModule -> tstBase;
  ent.cstBase = MEM_currentModule -> cstBase;

  EM_TRY{
	// Note: Assumes that a pipe is already open
	
	// This will set: ent.codeSpaceBeg = ent.codeSpaceEnd - codeSize
	LD_detail("loading code size\n");
	LD_CODE_LoadCodeSize(&ent);
  }EM_CATCH{
	LD_error("Failed to open query stream\n");
	EM_THROW(LD_LoadError);
  }
  
  EM_TRY{
	// Load new type skeletons contiguously to already loaded TST
	LD_detail("loading new type skeletons\n");
	LD_TYSKEL_LoadTst(&ent, 1);

	// Load new hidden constants contiguously to already loaded CST
	LD_detail("loading new constant symbols\n");
	LD_CONST_LoadCst(&ent, 1);


	ent.modSpaceBeg = ent.modSpaceEnd = AM_hreg;

	/* Read string tables */
	LD_detail("loading string table\n");
	LD_STRING_LoadStrings(&ent);

	// Read implication tables
	LD_detail("loading implication tables\n");
	LD_IMPLGOAL_LoadImplGoals(&ent, 1);

	/* Read hash tables */
	LD_detail("loading hashtables\n");
	LD_HASHTAB_LoadHashTabs(&ent, 1);

	/* Read bound variable tables */
	LD_detail("loading bvr tables\n");
	LD_BVRTAB_LoadBvrTabs(&ent);
  
	/* Read bytecode in query mode */
	LD_detail("loading code\n");
	LD_CODE_LoadCode(&ent, 1);

	LD_STRING_Cleanup();
	LD_IMPLGOAL_Cleanup();
	LD_HASHTAB_Cleanup();
	LD_BVRTAB_Cleanup();

	LD_FILE_ClosePipe();
	
  }EM_CATCH{
	LD_error("Failed to load compiled query\n");
	LD_FILE_ClosePipe();
	EM_THROW(LD_LoadError);
  }


  // In the event that the type skeleton or constant
  // tables have been reallocated, we must update
  // the values for the current module.
  // Note that tstSize and cstSize should
  // reflect the original size of the tables,
  // and therefore should not be changed.
  MEM_currentModule -> tstBase = ent.tstBase;
  MEM_currentModule -> cstBase = ent.cstBase;

  // Note: One could also call
  //   front_c.c:FRONT_initSymbolTableBases
  // but this seems cleaner.
  AM_tstBase = ent.tstBase;
  AM_cstBase = ent.cstBase;
    
  // Set heap register to end of module space
  // When a new query is loaded it will be reset to the top of the heap
  AM_hreg = (MemPtr)ent.modSpaceEnd;
  
  // We set the AM_preg register to the beginning of the code start,
  // which always looks like this (see also front/query_c.c):
  // 0x0:fail -                       <- ent.codeSpaceBeg
  // 0x8:try_me_else - #0 L0 
  // 0x18:...
  // In front/query_c.c, we skip over the first two instructions
  AM_preg = (MemPtr)ent.codeSpaceBeg;

  // We temporarily shrink the heap so that
  // code for the query does not get overwritten.
  // For this reason, we must recall the true location of
  // the end of the heap so that it may be reset for the next query.
  // Note: This is kind of a hack, but much simpler than putting the
  // code space before the module space, because the loader generally
  // assumes that the module space precedes the code space.
  LD_LOADQ_heapEnd = AM_heapEnd;
  AM_heapEnd = ent.codeSpaceBeg;
  
}
