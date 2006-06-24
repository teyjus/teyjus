#ifndef _TREE_H_
#define _TREE_H_

/////////////////////////////////////////////////////////////////
//Defines structures and methods for treating search trees//
/////////////////////////////////////////////////////////////////

struct TreeNode;

struct Tree{
	struct TreeNode* root;
	int numEntries;
};

extern void InitTree(struct Tree* t);

//Adds a item to the tree, if the key matches an existing node
//it returns that nodes data field.  Otherwise it creates a new
//node and sets it's data field to the number of elements in the
//tree, and returns that data value.
extern int Add(struct Tree* t, char* key);

//Matches the key to an existing value in the tree, removes it's node,
//and returns the nodes data value.  Returns -1 on failure.
extern int Remove(struct Tree* t, char* key);

#endif //_TREE_H_
