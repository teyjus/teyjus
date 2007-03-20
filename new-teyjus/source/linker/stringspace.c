
#include <stdlib.h>
#include "stringspace.h"
#include "module.h"
#include "vector.h"
#include "file.h"


#include <stdio.h>
#define DEBUG(x) printf("%s\n",x)
//////////////////////////////////////////////////////
//StringSpace Load and Write Code
//////////////////////////////////////////////////////
typedef Name TStringSpace_t;

struct Vector StringSpaces;

void InitTStringSpaces();
void LoadStringSpace(int i);
void LoadStringSpaces();
void WriteStringSpace(int i);
void WriteStringSpaces();

void InitTStringSpaces()
{
  LK_VECTOR_Init(&StringSpaces,128,sizeof(TStringSpace_t));
}

void LoadStringSpace(int i)
{
  TStringSpace_t* tmp=(TStringSpace_t*)LK_VECTOR_GetPtr(&StringSpaces,i);
  GetName(tmp);
}

void LoadStringSpaces()
{
  int i;
  
  TwoBytes count=CM->StringSpacecount=GET2();
  int offset=CM->StringSpaceoffset=LK_VECTOR_Grow(&StringSpaces,count);
  for(i=0;i<count;i++)
  {
    LoadStringSpace(offset+i);
  }
}

void WriteStringSpace(int i)
{
  TStringSpace_t* tmp=(TStringSpace_t*)LK_VECTOR_GetPtr(&StringSpaces,i);
  PutName(*tmp);
}

void WriteStringSpaces()
{
  int i;
  int size=LK_VECTOR_Size(&StringSpaces);
  PUT2(size);
  for(i=0;i<size;i++)
  {
    WriteStringSpace(i);
  }
}
