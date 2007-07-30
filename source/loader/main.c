#include <stdio.h>
#include "../system/memory.h"
#include "../tables/pervinit.h"
#include "loader.h"
#include "ld_message.h"

int main(int argc, char* argv[])
{
  LD_verbosity = 3;
  EM_TRY{
    printf("Beginning test...");
    MEM_memInit(1024*8);
    PERVINIT_tableInit();
    printf("Memory Initialized\n");
    LD_LOADER_Load(argv[1], 0);
    printf("Module Loaded\n");
  }EM_CATCH{
    printf("Caught an exception\n");
    return -1;
  }
  return 0;
}
