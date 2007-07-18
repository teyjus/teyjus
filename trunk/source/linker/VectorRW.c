#include "VectorRW.h"
#include "file.h"
#include "vector.h"
#include "module.h"

void LK_VECTOR_Read(int fd, struct Vector* vec, struct Module_st* CMData, Adjust_t* adj,void (*read_fn)(int fd, struct Module_st* CMData, void* entry))
{
  int i;
  int count=adj->count=LK_FILE_GET2(fd);
  if(count==0)
  {
    adj->offset=0;
    return;
  }
  int offset=adj->offset=LK_VECTOR_Grow(vec,count);
  void* base=LK_VECTOR_GetPtr(vec,offset);
  int objSize=vec->objSize;
  for(i=0;i<count;i++)
  {
    read_fn(fd,CMData,base+i*objSize);
  }
}

void LK_VECTOR_Write(int fd, struct Vector* vec,void (*write_fn)(int fd, void* entry))
{
  int i;
  int size=LK_VECTOR_Size(vec);
  LK_FILE_PUT2(fd,size);
  if(size==0) return;
  void* base = LK_VECTOR_GetPtr(vec,0);
  int objSize= vec->objSize;
  for(i=0;i<size;i++)
  {
    write_fn(fd,base+i*objSize);
  }
}
