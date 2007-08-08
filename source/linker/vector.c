#include <stdlib.h>
#include <stdio.h>
#include "vector.h"
#include "datatypes.h"
#include "../system/error.h"
#include "message.h"

#define chunksize 256

Word LK_VECTOR_Size(struct Vector* vec)
{
  return vec->usesize;
}

void LK_VECTOR_Init(struct Vector* vec, Word max, Word size)
{
  vec->data=NULL;
  vec->usesize=0;
  vec->maxsize=0;
  vec->objSize=size;
}

Word LK_VECTOR_Grow(struct Vector* vec, Word count)
{
  Word tmp=vec->usesize;
  vec->usesize+=count;
  if(vec->usesize>vec->maxsize)
  {
    vec->maxsize=(vec->usesize/chunksize +1)*chunksize;
    vec->data=EM_realloc(vec->data,vec->objSize*vec->maxsize);
  }
  return tmp;
}

void* LK_VECTOR_GetPtr(struct Vector* vec, Word index)
{
  if(index<0||index>=LK_VECTOR_Size(vec))
  {
    if(LK_VECTOR_Size(vec)==0 && index==0)
    {
      mutter("Getting start of empty vector\n");
      return NULL;
    }
    EM_THROW(LK_LinkError);
  }
  
  return (char*)vec->data + index*vec->objSize;
}

void LK_VECTOR_Free(struct Vector* vec)
{
  free(vec->data);
  vec->data=NULL;
  vec->usesize=0;
  vec->maxsize=0;
}

