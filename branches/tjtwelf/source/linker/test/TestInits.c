#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"
#include "linker/kind.h"
#include "linker/tyskel.h"
#include "linker/const.h"
#include "linker/stringspace.h"
#include "linker/implgoal.h"
#include "linker/hashtab.h"
#include "linker/bvrtab.h"
#include "linker/code.h"
#include "linker/importtab.h"

void __CUT__inits()
{
  InitTLKinds();
  InitTTySkels();
  InitTLConsts();
  InitTHConsts();
  LK_STRINGS_Init();
  InitTImplGoals();
  InitTHashTabs();
  InitTBvrTabs();
  InitTCode();
  InitTImportTabs();
}
