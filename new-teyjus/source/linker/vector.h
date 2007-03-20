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
//Initializes the vector at vec to hold elements
//of size size, and enough space to hold max elements
extern void LK_VECTOR_Init(struct Vector* vec, int max, size_t size);

extern int LK_VECTOR_Size(struct Vector* vec);
//Extends the vector at vec to hold count more elements.
//Preforms a resize on the vector if necessary.
//Returns the index of the first element of the added block.
extern int LK_VECTOR_Grow(struct Vector* vec, int count);

//Returns a pointer to the element in vec with the given index.
extern void* LK_VECTOR_GetPtr(struct Vector* vec, int index);

//Frees the memory used by the vector
extern void LK_VECTOR_Free(struct Vector* vec);

#endif //_VECTOR_H_
