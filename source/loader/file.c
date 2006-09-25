#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "file.h"

int fd;

void LD_FILE_GetString(char* buffer,int length)
{
	read(fd,buffer,length);
}

int LD_FILE_Open(char* modname, char* extension)
{
	char* buf=malloc(sizeof(char)*(strlen(modname)+strlen(extension)+1));
	if(buf==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	sprintf(buf,"%s%s",modname,extension);
	
	
	fd=open(buf,O_RDONLY,0000);
	if(fd==-1)
	{
		perror(buf);
		return -1;
	}
	
	return 0;
}

int LD_FILE_Close()
{
	if(-1!=close(fd))
		return 0;
	
	return -1;
}

int LD_FILE_Exists(char* modname, char* extension)
{
	char* filepath=malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
	if(filepath==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	sprintf(filepath,"./%s%s",modname,extension);
	
	struct stat buf;
	if(-1==stat(filepath,&buf))
	{
		perror(filepath);
		free(filepath);
		return -1;
	}
	
	free(filepath);
	return 0;
}

int LD_FILE_Link(char* modname)
{
	return -1;
}

int LD_FILE_ModTime(char* modname, char* extension)
{
	char* filepath=malloc(sizeof(char)*(strlen(modname)+strlen(extension)+3));
	if(filepath==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	sprintf(filepath,"./%s%s",modname,extension);
	
	struct stat buf;
	if(-1==stat(filepath,&buf))
	{
		perror(filepath);
		free(filepath);
		return -1;
	}
	
	free(filepath);
	return buf.st_mtime;
}


Word LD_FILE_GETWORD()
{
	//int tmp;
	//read(fd,&tmp,sizeof(tmp));
	//return (Word)tmp;
	return (Word)LD_FILE_GET4();
}

FourBytes LD_FILE_GET4()
{
	FourBytes tmp;
	read(fd,&tmp,sizeof(tmp));
	return ntohl(tmp);
}

TwoBytes LD_FILE_GET2()
{
	TwoBytes tmp;
	read(fd,&tmp,sizeof(tmp));
	return ntohs(tmp);
}

Byte LD_FILE_GET1()
{
	Byte tmp;
	read(fd,&tmp,sizeof(tmp));
	return tmp;
}
