#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tree.h"

struct TreeNode{
	struct TreeNode* left;
	struct TreeNode* right;
	struct TreeNode* parent;
	char* key;
	int data;
};

//void Init(struct Tree* t);
struct TreeNode* CreateNode(struct Tree* t, char* key);
int Add(struct Tree* t, char* key);
int Remove(struct Tree* t, char* key);

void InitTree(struct Tree* t)
{
	t->root=NULL;
	t->numEntries=0;
	return;
}

struct TreeNode* CreateNode(struct Tree* t, char* key)
{
	struct TreeNode* tmp=malloc(sizeof(struct TreeNode));
	if(tmp==NULL)
	{
		perror("Memory Allocation Failed");
		exit(0);
	}
	tmp->left=NULL;
	tmp->right=NULL;
	tmp->parent=NULL;
	tmp->key=malloc(strlen(key));
	strcpy(tmp->key,key);
	tmp->data=t->numEntries++;
	
	return tmp;
}

int Add(struct Tree* t, char* key)
{
	int cmp;
	struct TreeNode* tmp=t->root;
	if(tmp==NULL)
	{
		tmp=t->root=CreateNode(t,key);
	}
	else
	{
		cmp=strcmp(tmp->key,key);
		while(0!=cmp)
		{
			if(0<cmp)
			{
				if(tmp->right==NULL)
				{
					tmp->right=CreateNode(t,key);
					tmp->right->parent=tmp;
				}
				tmp=tmp->right;
			}
			else
			{
				if(tmp->left==NULL)
				{
					tmp->left=CreateNode(t,key);
					tmp->left->parent=tmp;
				}
				tmp=tmp->left;
			}
			cmp=strcmp(tmp->key,key);
		}
	}
	return tmp->data;
}

int Remove(struct Tree* t, char* key)
{
	struct TreeNode* tmp=t->root;
	struct TreeNode* tmp2;
	int cmp;
	if(tmp==NULL)
	{
		return -1;
	}
	else
	{
		cmp=strcmp(tmp->key,key);
		while(0!=cmp)
		{
			if(0<cmp)
			{
				if(tmp->right==NULL)
					return -1;
				tmp=tmp->right;
			}
			else
			{
				if(tmp->left==NULL)
					return -1;
				tmp=tmp->left;
			}
			cmp=strcmp(tmp->key,key);
		}
		
		if(tmp->right==NULL)
		{
			tmp->right=tmp->left;
		}
		else
		{
			tmp2=tmp->right;
			while(tmp2->left!=NULL)
			{
				tmp2=tmp->left;
			}
			tmp2->left=tmp->left;
		}
		
		if(tmp->parent==NULL)
		{
			t->root=tmp->right;
		}
		else if(tmp->parent->right==tmp)
		{
			tmp->parent->right=tmp->right;
		}
		else
		{
			tmp->parent->left=tmp->right;
		}
		
		cmp=tmp->data;
		free(tmp->key);
		free(tmp);
		return cmp;
	}
}
