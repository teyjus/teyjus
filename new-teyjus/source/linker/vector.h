#ifndef _VECTOR_H_
#define _VECTOR_H_
#include <stddef.h>
/////////////////////////////////////////////////////////////////
//Defines structures and methods for treating extendable arrays//
/////////////////////////////////////////////////////////////////

struct Vector{
  void*  entry;
  int    numEntries;
  int    maxEntries;
  size_t  entrySize;
};

//Initializes the vector at vec to hold elements
//of size size, and enough space to hold max elements
extern int InitVec(struct Vector* vec, int max, size_t size);

//Extends the vector at vec to hold count more elements.
//Preforms a resize on the vector if necessary.
//Returns the index of the first element of the added block.
extern int Extend(struct Vector* vec, int count);

//Returns a pointer to the element in vec with the given index.
extern void* Fetch(struct Vector* vec, int index);

//Frees the memory used by the vector
extern void Destroy(struct Vector* vec);

#endif //_VECTOR_H_
