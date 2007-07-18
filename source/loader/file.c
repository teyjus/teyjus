#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "file.h"
#include "loader.h"

int fd;

void LD_FILE_GetString(char* buffer,int length)
{
	read(fd,buffer,length);
    buffer[length]='\0';
}

void LD_FILE_Open(char* modname, char* extension)
{
  char* buf=EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+1));
  sprintf(buf,"%s%s",modname,extension);
  
  
  fd=open(buf,O_RDONLY,0000);
  if(fd==-1)
  {
    perror(buf);
    EM_THROW(LD_FILE_OpenError);
  }
}

void LD_FILE_Close()
{
  if(-1!=close(fd))
    EM_THROW(LD_FILE_CloseError);
}

int LD_FILE_Exists(char* modname, char* extension)
{
  char* filepath=EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
  sprintf(filepath,"./%s%s",modname,extension);
  
  struct stat buf;
  if(-1==stat(filepath,&buf))
  {
    perror(filepath);
    free(filepath);
    return 0;
  }
  
  free(filepath);
  return 1;
}

void LD_FILE_Link(char* modname)
{
  EM_THROW(LD_FILE_LinkFailure);
}

int LD_FILE_ModTime(char* modname, char* extension)
{
  char* filepath=EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
  sprintf(filepath,"./%s%s",modname,extension);
  
  struct stat buf;
  if(-1==stat(filepath,&buf))
  {
    perror(filepath);
    free(filepath);
    EM_THROW(LD_FILE_OpenError);
  }
  
  free(filepath);
  return buf.st_mtime;
}


Word LD_FILE_GETWORD()
{
  int tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
  return (Word)tmp;
  //return (Word)LD_FILE_GET4();
}

/*
FourBytes LD_FILE_GET4()
{
	FourBytes tmp;
	read(fd,&tmp,sizeof(tmp));
	return ntohl(tmp);
}*/

TwoBytes LD_FILE_GET2()
{
  TwoBytes tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
  return tmp;
  //return ntohs(tmp);
}

Byte LD_FILE_GET1()
{
  Byte tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
  return tmp;
}

//#ifdef DEBUG
int pfd;

void LD_FILE_OpenPipe()
{
  int m_pipe[2];
  if(-1==pipe(m_pipe))
    EM_THROW(LD_FILE_OpenError);
  fd=m_pipe[0];
  pfd=m_pipe[1];
}

void LD_FILE_PipePUT1(Byte b)
{
  write(pfd,&b,sizeof(b));
}

void LD_FILE_PipePUT2(TwoBytes s)
{
  write(pfd,&s,sizeof(s));
}

void LD_FILE_PipePUTWORD(Word w)
{
  write(pfd,&w,sizeof(w));
}

void LD_FILE_PipePutString(char* str)
{
  char len=strlen(str);
  write(pfd,&len,sizeof(len));
  write(pfd,str,len);
}

void LD_FILE_ClosePipe()
{
  close(fd);
  close(pfd);
}

//#endif
