#include <stdio.h>
#include "../system/memory.h"
#include "loader.h"

int main()
{
	printf("Beginning test...");
	MEM_memInit(1024*8);
	printf("Memory Initialized\n");
	LD_LOADER_Load("test");
	printf("Module Loaded\n");
	
	return 0;
}
