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
#include <sys/stat.h>
#include <stdlib.h>
#include "../include/standardlib.h"
#include <stdio.h>
#include <string.h>
#include "file.h"
#include "../system/error.h"
#include "linker_message.h"

#define DEBUG(x) printf("%s\n",x)
#define SWAPENDIAN

char* LK_FILE_LinkCodeExt=".lp";
char* LK_FILE_ByteCodeExt=".lpo";

int LK_FILE_OpenInput(char* modname, char* extension)
{
  int fd;
  char* tjpath;
  char* tjpath_; /* We need a copy since strtok modifies its argument */
  char* dir;
  char* buf=(char *)EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
	
  fd=open(buf,O_RDONLY|O_BINARY,0000);
  if(fd==-1)
  {
    /* Restrictive hack: works only on Unix
     * To be fixed with C, or, better, rewrite all the linker in OCaml
     * to do this easily */
    tjpath=getenv("TJPATH");
    if (tjpath == NULL) tjpath=".";
    tjpath_=strdup(tjpath);

    dir=strtok(tjpath_,":");
    
    while (dir != NULL) {
      buf=(char *)EM_malloc(strlen(modname)+strlen(extension)+strlen(dir)+2);
      sprintf(buf,"%s/%s%s",dir,modname,extension);
      fd=open(buf,O_RDONLY|O_BINARY,0000);
      if(fd!=-1) 
        return fd;
      dir=strtok(NULL,":");
    }
    
    bad("Couldn't open file %s for reading.\n",buf); 
    EM_THROW(LK_LinkError);

  }
  return fd;
}

int LK_FILE_OpenOutput(char* modname, char* extension)
{
  int fd;
  char* buf=(char *)EM_malloc(strlen(modname)+strlen(extension)+1);
  sprintf(buf,"%s%s",modname,extension);
  
  fd=open(buf,O_WRONLY|O_CREAT|O_TRUNC|O_BINARY,0666);
  if(fd==-1)
  {
    bad("Couldn't open file %s for writing.\n",buf); 
    EM_THROW(LK_LinkError);
  }
  return fd;
}

void LK_FILE_xPipe(int fd[2])
{
  if(pipe(fd))
  {
    bad("Couldn't open pipe.\n"); 
    EM_THROW(LK_LinkError);
  }
}

void LK_FILE_Close(int fd)
{
  close(fd);///\todo handle close error?
}

Word LK_FILE_GETWord(int fd)
{
  Word tmp;
  read(fd,&tmp,sizeof(tmp));
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

INT4 LK_FILE_GET4(int fd)
{
  INT4 tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_32(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

TwoBytes LK_FILE_GET2(int fd)
{
  TwoBytes tmp;
  read(fd,&tmp,sizeof(tmp));
#if BYTE_ORDER == LITTLE_ENDIAN
  return bswap_16(tmp);
#elif BYTE_ORDER == BIG_ENDIAN
  return tmp;
#endif
}

Byte LK_FILE_GET1(int fd)
{
  Byte tmp;
  read(fd,&tmp,sizeof(tmp));
  return tmp;
}

char* LK_FILE_GetString(int fd)
{
  char* tmp;
  int size=LK_FILE_GET4(fd);
  //fprintf(stderr, "Name:%d ",size);//DEBUG
  tmp=(char *)EM_malloc(size+1);
  read(fd,tmp,size);
  tmp[size]='\0';
  //fprintf(stderr, "\"%s\"\n",tmp);//DEBUG
  return tmp;
}

void LK_FILE_PUT1(int fd, Byte x)
{
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT2(int fd, TwoBytes x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
    x=bswap_16(x);
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUT4(int fd, INT4 x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
  x=bswap_32(x);
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PUTN(int fd, void* data,int n)
{
  write(fd,data,n);
}

void LK_FILE_PUTWord(int fd, Word x)
{
#if BYTE_ORDER == LITTLE_ENDIAN
# if __WORDSIZE == 32
  x=(Word)bswap_32((unsigned int)x);
# elif __WORDSIZE == 64
  x=bswap_64(x);
# endif
#elif BYTE_ORDER == BIG_ENDIAN
#endif
  write(fd,&x,sizeof(x));
}

void LK_FILE_PutString(int fd, char* str)
{
  int size=strlen(str);
  LK_FILE_PUT4(fd,size);
  write(fd,str,size);
}

