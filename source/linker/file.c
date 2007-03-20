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

char* LK_FILE_LinkCodeExt=".lpo";
char* LK_FILE_ByteCodeExt=".lpa";

int LK_FILE_OpenInput(char* modname, char* extension)
{
  char* buf=EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
  
  int fd=open(buf,O_RDONLY,0000);
  if(fd==-1)
  {
    EM_THROW(LK_LinkError);
  }
  return fd;
}

int LK_FILE_OpenOutput(char* modname, char* extension)
{
  char* buf=EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
  
  int fd=open(buf,O_WRONLY|O_CREAT|O_TRUNC,0666);
  if(fd==-1)
  {
    EM_THROW(LK_LinkError);
  }
  return fd;
}

void LK_FILE_xPipe(int fd[2])
{
  if(pipe(fd))
    EM_THROW(LK_LinkError);
}

void LK_FILE_Close(int fd)
{
  close(fd);///\todo handle close error?
}

struct FileNode{
  int fd;
  struct FileNode* next;
};

struct FileNode* InFile=NULL;

int ofd;

int PeekInput()
{
  return InFile->fd;
}

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

int PeekOutput()
{
  return ofd;
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
  free(buf);
}

void CloseOutput()
{
  close(ofd);
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
  tmp=name->string=EM_malloc(size+1);
  read(InFile->fd,tmp,size);
  tmp[size]='\0';
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
  LK_FILE_PipePUT1(size);
  
  write(ofd,name.string,size);
}

Word LK_FILE_GETWord(int fd)
{
  Word tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

INT4 LK_FILE_GET4(int fd)
{
  INT4 tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

TwoBytes LK_FILE_GET2(int fd)
{
  TwoBytes tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

Byte LK_FILE_GET1(int fd)
{
  Byte tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

Name* LK_FILE_GetName(int fd, Name* name)
{
/*  if(name==NULL)
  return NULL;*/
  char* tmp;
  Byte size=name->size=LK_FILE_GET1(fd);
  //printf("Name:%d ",size);//DEBUG
  tmp=name->string=EM_malloc(size+1);
  read(fd,tmp,size);
  tmp[size]='\0';
  //printf("\"%s\"\n",tmp);//DEBUG
  return name;
}

char* LK_FILE_GetString(int fd)
{
  char* tmp;
  Byte size=LK_FILE_GET1(fd);
  //printf("Name:%d ",size);//DEBUG
  tmp=EM_malloc(size+1);
  read(fd,tmp,size);
  tmp[size]='\0';
  //printf("\"%s\"\n",tmp);//DEBUG
  return tmp;
}

void LK_FILE_PUT1(int fd, Byte x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT2(int fd, TwoBytes x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT4(int fd, INT4 x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUTN(int fd, void* data,int n)
{
  write(fd,data,n);
}

void LK_FILE_PUTWord(int fd, Word x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PutName(int fd, Name name)
{
  Byte size=name.size;
  //printf("Putting name %d : \"%s\"\n",size,name.string);
  LK_FILE_PUT1(fd,size);
  write(fd,name.string,size);
}

void LK_FILE_PutString(int fd, char* str)
{
  Byte size=strlen(str);
  LK_FILE_PUT1(fd,size);
  write(fd,str,size);
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

void LK_FILE_PipePutName(char* str)
{
  Byte size=strlen(str);
  LK_FILE_PipePUT1(size);
  
  write(pifd,str,size);
}


void LK_FILE_PipeInputClose()
{
  close(pifd);
}


int pofd;

void LK_FILE_PipeOutput()
{
  int pipes[2];
  if(pipe(pipes))
    EM_THROW(LK_LinkError);
  
  ofd=pipes[1];
  pofd=pipes[0];
}

Word LK_FILE_PipeGETWord()
{
  Word tmp;
  read(pofd,&tmp,sizeof(tmp));
  return tmp;
}

INT4 LK_FILE_PipeGET4()
{
  INT4 tmp;
  read(pofd,&tmp,sizeof(tmp));
  return tmp;
}

TwoBytes LK_FILE_PipeGET2()
{
  TwoBytes tmp;
  read(pofd,&tmp,sizeof(tmp));
  return tmp;
}

Byte LK_FILE_PipeGET1()
{
  Byte tmp;
  read(pofd,&tmp,sizeof(tmp));
  return tmp;
}

Name* LK_FILE_PipeGetName(Name* name)
{
/*  if(name==NULL)
  return NULL;*/
  char* tmp;
  Byte size=name->size=LK_FILE_PipeGET1();
  tmp=name->string=EM_malloc(size+1);
  read(pofd,tmp,size);
  tmp[size]='\0';
  return name;
}

void LK_FILE_PipeOutputClose()
{
  close(pofd);
}
//#endif
