#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "file.h"

#define DEBUG(x) printf("%s\n",x)
#define SWAPENDIAN

struct FileNode{
  int fd;
  struct FileNode* next;
};

struct NameNode{
  char* name;
  struct NameNode* next;
};

struct FileNode* InFile=NULL;

struct NameNode* UsedFile=NULL;

int files=0;

int ofd;

void PushInput(char* modname)
{
  char* buf=malloc(sizeof(char)*1024);
  if(buf==NULL)
  {
    perror("Malloc Failure");
    exit(0);
  }
  sprintf(buf,"%s.lp",modname);
  
  
  struct FileNode* tmp = malloc(sizeof(struct FileNode));
  if(tmp==NULL)
  {
    perror("Malloc Failure");
    exit(0);
  }
  
  
  tmp->fd=open(buf,O_RDONLY,0000);
  if(tmp->fd==-1)
  {
    perror(buf);
    exit(0);
  }
  
  
  struct NameNode* tmp2 = malloc(sizeof(struct NameNode));
  if(tmp2==NULL)
  {
    perror("Malloc Failure");
    exit(0);
  }
  
  tmp2->name=malloc(strlen(modname)+1);
  if(tmp2->name==NULL)
  {
    perror("Malloc Failure");
    exit(0);
  }
  
  strcpy(tmp2->name,modname);
  tmp->next=InFile;
  tmp2->next=UsedFile;
  InFile=tmp;
  UsedFile=tmp2;
  files++;
}

void PopInput()
{
  close(InFile->fd);
  struct FileNode* tmp=InFile->next;
  free(InFile);
  InFile=tmp;
}

void SetOutput(char* modname)
{
  char* buf=malloc(sizeof(char)*1024);
  if(buf==NULL)
  {
    perror("Malloc Failure");
    exit(0);
  }
  sprintf(buf,"%s.bc",modname);
  
  ofd=open(buf,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if(ofd==-1)
  {
    perror(buf);
    exit(0);
  }
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

int GETWord()
{
  //int tmp;
  //read(InFile->fd,&tmp,sizeof(tmp));
  //return tmp;
  return GET4();
}

INT4 GET4()
{
  INT4 tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return ntohl(tmp);
}

TwoBytes GET2()
{
  TwoBytes tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return ntohs(tmp);
}

Byte GET1()
{
  Byte tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return tmp;
}

Name* GetName(Name* name)
{
/*  if(name==NULL)
    return NULL;*/
  char* tmp;
  Byte size=name->size=GET1();
  //printf("Name:%d ",size);//DEBUG
  tmp=name->string=malloc(size);
  if(tmp==NULL)
  {
    perror("Memory Allocation Failed");
    exit(0);
  }
  read(InFile->fd,tmp,size);
  //printf("\"%s\"\n",tmp);//DEBUG
  return name;
}

void PUT1(Byte x)
{
  write(ofd,&x,sizeof(x));
}

void PUT2(TwoBytes x)
{
  TwoBytes tmp = htons(x);
  write(ofd,&tmp,sizeof(x));
}

void PUT4(INT4 x)
{
  INT4 tmp = htonl(x);
  write(ofd,&tmp,sizeof(x));
}

void PUTN(void* data,int n)
{
  write(ofd,data,n);
}

void PUTWord(int x)
{
  PUT4(x);
  //write(ofd,&x,sizeof(x));
}

void PutName(Name name)
{
  Byte size=name.size;
  PUT1(size);
  
  write(ofd,name.string,size);
}
