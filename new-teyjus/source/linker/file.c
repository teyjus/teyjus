#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "file.h"
#include "../system/error.h"

#define DEBUG(x) printf("%s\n",x)
#define SWAPENDIAN

struct FileNode{
  int fd;
  struct FileNode* next;
};

struct FileNode* InFile=NULL;

int ofd;

void PushInput(char* modname)
{
  char* buf=EM_malloc(sizeof(char)*1024);
  sprintf(buf,"%s.lp",modname);
  
  struct FileNode* tmp = EM_malloc(sizeof(struct FileNode));
  
  tmp->fd=open(buf,O_RDONLY,0000);
  if(tmp->fd==-1)
  {
    perror(buf);
    exit(0);
  }
  tmp->next=InFile;
  InFile=tmp;
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
  char* buf=EM_malloc(sizeof(char)*1024);
  sprintf(buf,"%s.bc",modname);
  
  ofd=open(buf,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if(ofd==-1)
  {
    perror(buf);
    exit(0);
  }
}

Word GETWord()
{
  Word tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return tmp;
}

INT4 GET4()
{
  INT4 tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return tmp;
}

TwoBytes GET2()
{
  TwoBytes tmp;
  read(InFile->fd,&tmp,sizeof(tmp));
  return tmp;
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
  tmp=name->string=EM_malloc(size);
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
  write(ofd,&x,sizeof(x));
}

void PUT4(INT4 x)
{
  write(ofd,&x,sizeof(x));
}

void PUTN(void* data,int n)
{
  write(ofd,data,n);
}

void PUTWord(Word x)
{
  write(ofd,&x,sizeof(x));
}

void PutName(Name name)
{
  Byte size=name.size;
  PUT1(size);
  
  write(ofd,name.string,size);
}

//#ifdef DEBUG
int pifd=-1;

void LK_FILE_PushPipeInput()
{
  int pipes[2];
  struct FileNode* tmp = EM_malloc(sizeof(struct FileNode));
  if(pipe(pipes))
    EM_THROW(LK_LinkError);
  
  pifd=pipes[1];
  
  tmp->fd=pipes[0];
  tmp->next=InFile;
  InFile=tmp;
}

void LK_FILE_PipePUT1(Byte x)
{
  write(pifd,&x,sizeof(x));
}

void LK_FILE_PipePUT2(TwoBytes x)
{
  write(pifd,&x,sizeof(x));
}

void LK_FILE_PipePUT4(INT4 x)
{
  write(pifd,&x,sizeof(x));
}

void LK_FILE_PipePUTWord(Word x)
{
  write(pifd,&x,sizeof(x));
}


void LK_FILE_PipePUTN(void* data,int n)
{
  write(pifd,data,n);
}

void LK_FILE_PipePutName(Name name)
{
  Byte size=name.size;
  LK_FILE_PipePUT1(size);
  
  write(pifd,name.string,size);
}
//#endif
