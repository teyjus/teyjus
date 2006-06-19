#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

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

void PushInput(char* modname)
{
	char buf=malloc(sizeof(char)*1024);
	if(buf==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	sprintf(buf,"%s.bc",modname);
	struct FileNode* tmp = malloc(sizeof(FileNode));
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
	struct NameNode* tmp2 = malloc(sizeof(NameNode));
	if(tmp2==NULL)
	{
		perror("Malloc Failure");
		exit(0);
	}
	tmp2->name=buf;
	tmp->next=InFile;
	tmp2->next=UsedFile;
	Infile=tmp;
	UsedFile=tmp2;
}

void PopInput()
{
	close(InFile->fd);
	struct FileNode* tmp=Infile->next;
	free(Infile);
	Infile=tmp;
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

INT2 GET2();
{
	INT2 tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}

INT1 GET1();
{
	INT1 tmp;
	read(InFile->fd,&tmp,sizeof(tmp));
	return tmp;
}
