#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/tree.h"

void __CUT__treecheck()
{
  EM_TRY
  {
    MarkInd tmp;
    void* MyTree=NULL;
    tmp.gl_flag=GLOBAL;
    tmp.index=0;
    
    tmp.index++;
    LK_TREE_Add(&MyTree, "Foo", tmp);
    tmp.index++;
    LK_TREE_Add(&MyTree, "Bar", tmp);
    tmp.index++;
    LK_TREE_Add(&MyTree, "Baz", tmp);
    tmp.index++;
    LK_TREE_Add(&MyTree, "Boo", tmp);
    
    tmp=LK_TREE_Retrieve(&MyTree,"Bar");
    ASSERT(tmp.index==2,"Fetch check");
    tmp=LK_TREE_Retrieve(&MyTree,"Baz");
    ASSERT(tmp.index==3,"Fetch check");
    tmp=LK_TREE_Retrieve(&MyTree,"Bar");
    ASSERT(tmp.index==2,"Fetch check");
    
    LK_TREE_Empty(&MyTree);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
