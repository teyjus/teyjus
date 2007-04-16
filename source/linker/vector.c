#include <stdlib.h>
#include <stdio.h>
#include "vector.h"
#include "datatypes.h"
#include "../system/error.h"

// #define obstack_chunk_alloc EM_malloc
// #define obstack_chunk_free free
// 
// void LK_obstack_alloc_failed_handler(void)
// {
//   exit(0);
// }
// 
// int LK_VECTOR_Size(struct Vector* vec)
// {
//   return obstack_object_size(&(vec->obs))/vec->objSize;
// }
// 
// void LK_VECTOR_Init(struct Vector* vec,int max,size_t size)
// {
//   obstack_alloc_failed_handler=&LK_obstack_alloc_failed_handler;
//   obstack_init(&(vec->obs));
//   vec->objSize=size;
// }
// 
// int LK_VECTOR_Grow(struct Vector* vec, int count)
// {
//   int tmp=LK_VECTOR_Size(vec);
//   obstack_blank(&(vec->obs),count*vec->objSize);
//   return tmp;
// }
// 
// void* LK_VECTOR_GetPtr(struct Vector* vec, int index)
// {
//   if(index<0||index>=LK_VECTOR_Size(vec))
//   {
//     EM_THROW(LK_LinkError);
//   }
//   
//   return obstack_base(&(vec->obs)) + index*vec->objSize;
// }
// 
// void LK_VECTOR_Free(struct Vector* vec)
// {
//   void* tmp = obstack_finish(&(vec->obs));
//   obstack_free(&(vec->obs),tmp);
// }

#define chunksize 256

int LK_VECTOR_Size(struct Vector* vec)
{
  return vec->usesize;
}

void LK_VECTOR_Init(struct Vector* vec,int max,size_t size)
{
  vec->data=NULL;
  vec->usesize=0;
  vec->maxsize=0;
  vec->objSize=size;
}

int LK_VECTOR_Grow(struct Vector* vec, int count)
{
  int tmp=vec->usesize;
  vec->usesize+=count;
  if(vec->usesize>vec->maxsize)
  {
    vec->maxsize=(vec->usesize/chunksize +1)*chunksize;
    vec->data=EM_realloc(vec->data,vec->objSize*vec->maxsize);
  }
  return tmp;
}

void* LK_VECTOR_GetPtr(struct Vector* vec, int index)
{
  if(index<0||index>=LK_VECTOR_Size(vec))
  {
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

