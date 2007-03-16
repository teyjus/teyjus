
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
  InitVec(&StringSpaces,128,sizeof(TStringSpace_t));
}

void LoadStringSpace(int i)
{
  TStringSpace_t* tmp=(TStringSpace_t*)Fetch(&StringSpaces,i);
  GetName(tmp);
}

void LoadStringSpaces()
{
  int i;
  
  TwoBytes count=CM->StringSpacecount=GET2();
  int offset=CM->StringSpaceoffset=Extend(&StringSpaces,count);
  for(i=0;i<count;i++)
  {
    LoadStringSpace(offset+i);
  }
}

void WriteStringSpace(int i)
{
  TStringSpace_t* tmp=(TStringSpace_t*)Fetch(&StringSpaces,i);
  PutName(*tmp);
}

void WriteStringSpaces()
{
  int i;
  PUT2(StringSpaces.numEntries);
  for(i=0;i<StringSpaces.numEntries;i++)
  {
    WriteStringSpace(i);
  }
}
