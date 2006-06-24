#include <stdlib.h>
#include "vector.h"

int InitVec(struct Vector* vec,int max,size_t size)
{
	vec->numEntries=0;
	vec->maxEntries=max;
	vec->entrySize=size;
	if(size>0)
	{
		vec->entry=malloc(max*size);
	
		if(vec->entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	else
		vec->entry=NULL;
}

int Extend(struct Vector* vec, int count)
{
	int tmp=vec->numEntries;
	vec->numEntries+=count;
	if(vec->numEntries>vec->maxEntries)
	{
		do{
			vec->maxEntries*=2;
		}while(vec->numEntries>vec->maxEntries);
		
		vec->entry=realloc(vec->entry,vec->maxEntries*vec->entrySize);
		if(vec->entry==NULL)
		{
			perror("Memory Allocation Failed");
			exit(0);
		}
	}
	return tmp;
}

void* Fetch(struct Vector* vec, int index)
{
	if(index<0||index>=vec->numEntries)
	{
		return NULL;
	}
	
	return vec->entry+index*vec->entrySize;
}

void Destroy(struct Vector* vec)
{
	if(vec->entry!=NULL)
	{
		free(vec->entry);
		vec->entry=NULL;
	}
	vec->maxEntries=0;
	vec->numEntries=0;
}
