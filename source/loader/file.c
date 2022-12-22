//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "../include/standardlib.h"
#include "ld_message.h"
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
  char* buf=(char *)EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+1));
  sprintf(buf,"%s%s",modname,extension);
  
  fd=open(buf,O_RDONLY|O_BINARY,0000);
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
  struct stat buf;
  char* filepath=(char *)EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
  sprintf(filepath,"./%s%s",modname,extension);
  
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
  struct stat buf;
  char* filepath=(char *)EM_malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
  sprintf(filepath,"./%s%s",modname,extension);
  
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
  Word tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
#if BYTE_ORDER == LITTLE_ENDIAN
# if __WORDSIZE == 32
  return (Word)bswap_32((unsigned int)tmp);
# elif __WORDSIZE == 64
  return bswap_64(tmp);
# endif
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}


int LD_FILE_GET4()
{
  int tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_32(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}


TwoBytes LD_FILE_GET2()
{
  TwoBytes tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_16(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

Byte LD_FILE_GET1()
{
  Byte tmp;
  if(sizeof(tmp)!=read(fd,&tmp,sizeof(tmp)))
    EM_THROW(LD_FILE_ReadError);
  return tmp;
}

  
int pfd;

void LD_FILE_OpenPipe()
{
  int m_pipe[2];
  if(-1==pipe(m_pipe))
    EM_THROW(LD_FILE_OpenError);
  fd=m_pipe[0];
  pfd=m_pipe[1];
}

int LD_FILE_GetPipeIn(){
  return pfd;
}

//#ifdef DEBUG
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
