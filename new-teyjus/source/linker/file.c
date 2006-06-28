#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include "file.h"

#define DEBUG(x) printf("%s\n",x)

struct FileNode{
	int fd;
	struct FileNode* next;
};

struct NameNode{
	char* name;
	struct NameNode* next;
};

struct FileNode* InFile;

struct NameNode* UsedFile;

int ofd;

void PushInput(char* modname)
{
	char* buf=malloc(sizeof(char)*1024);
	if(buf==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	sprintf(buf,"%s.bc",modname);
	
	
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
	
	
	tmp2->name=buf;
	tmp->next=InFile;
	tmp2->next=UsedFile;
	InFile=tmp;
	UsedFile=tmp2;
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
	sprintf(buf,"%s.lc",modname);
	
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
	while(tmp!=NULL)
	{
		i=strlen(tmp->name);
		PUT1(i);
		while(i>0)
		{
			PUT1(tmp->name[i]);
			i--;
		}
	}
}

int GETWORD()
{
	int tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}

INT4 GET4()
{
	INT4 tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}

INT2 GET2()
{
	INT2 tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}

INT1 GET1()
{
	INT1 tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}

Name* GetName(Name* name)
{
/*	if(name==NULL)
		return NULL;*/
	char* tmp;
	int i=0;
	INT1 size=name->size=GET1();
	printf("Name:%d ",size);
	fflush(stdout);
	tmp=name->string=malloc(size);
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	read(InFile->fd,tmp,size);
	printf("\"%s\"\n",tmp);
	return name;
}

void PUT1(INT1 x)
{
	write(ofd,&x,sizeof(x));
}

void PUT2(INT2 x)
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

void PUTWORD(int x)
{
	write(ofd,&x,sizeof(x));
}

void PutName(Name name)
{
	int j;
	INT1 size=name.size;
	PUT1(size);
	
	write(ofd,name.string,size);
}
