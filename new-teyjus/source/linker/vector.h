#ifndef _VECTOR_H_
#define _VECTOR_H_
#include <stddef.h>
#include <obstack.h>
/////////////////////////////////////////////////////////////////
//Defines structures and methods for treating extendable arrays//
/////////////////////////////////////////////////////////////////

struct Vector{
  struct obstack obs;
  size_t objSize;
};

/**
\brief Initialize a vector for use.
\arg vec The vector to initialize.
\arg size The size of the elements the vector holds.
\arg max ignored
**/
extern void LK_VECTOR_Init(struct Vector* vec, int max, size_t size);

/**
\brief Get the number of elements contained by a vector.
**/
extern int LK_VECTOR_Size(struct Vector* vec);

/**
\brief Increase the size of a vector
\arg vec The vector
\arg count The number of elements to add to the vector.
**/
extern int LK_VECTOR_Grow(struct Vector* vec, int count);

/**
\brief Get a pointer to an element of a vector.
\arg vec The vector
\arg index The index of the element to retrieve.
\note Elements are assumed to be allocated contiguosly, so LK_VECTOR_GetPtr(v,0)+elSize*10 = LK_VECTOR_GetPtr(v,10)
\note Calling LK_VECTOR_Grow may invalidate the pointer returned.
**/
extern void* LK_VECTOR_GetPtr(struct Vector* vec, int index);

/**
\brief Free the memory used by a vector.
\arg vec The vector to free.
**/
extern void LK_VECTOR_Free(struct Vector* vec);

#endif //_VECTOR_H_
