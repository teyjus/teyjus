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

void WriteDependencies(int fd)
{
  struct NameNode* tmp=UsedFile;
  LK_FILE_PUT2(fd,files);
  while(tmp!=NULL)
  {
    LK_FILE_PutString(fd,tmp->name);
    tmp=tmp->next;
  }
}

