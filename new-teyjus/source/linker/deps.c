#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "file.h"

struct NameNode{
  char* name;
  struct NameNode* next;
};

struct NameNode* UsedFile=NULL;
int files=0;

void AddDependency(char* modname)
{
  struct NameNode* tmp2 = EM_malloc(sizeof(struct NameNode));
  tmp2->name=EM_malloc(strlen(modname)+1);
  
  strcpy(tmp2->name,modname);
  
  tmp2->next=UsedFile;
  
  UsedFile=tmp2;
  files++;
}

void WriteDependencies()
{
  int i;
  struct NameNode* tmp=UsedFile;
  PUT2(files);
  Name name;
  while(tmp!=NULL)
  {
    name.string=tmp->name;
    name.size=strlen(tmp->name)+1;
    PutName(name);
    tmp=tmp->next;
  }
}

