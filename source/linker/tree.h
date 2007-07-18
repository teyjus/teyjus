#ifndef _TREE_H_
#define _TREE_H_

#include "datatypes.h"
/**
\brief Add a node to a tree.
\arg root The address of a pointer to the tree root.  Use a pointer to null for an empty tree.
\arg key The string which the key is organized in the root by.
\arg ind The data to associate with the key.
\throw LK_OutOfMemory
**/
extern void LK_TREE_Add(void** root, char* key, MarkInd ind);

/**
\brief Get the data associated with name key.
\throw LK_LinkError
**/
extern MarkInd LK_TREE_Retrieve(void** root, char* key);

/**
 \brief Free the memory used by the tree at root.
**/
extern void LK_TREE_Empty(void** root);
#endif //_TREE_H_
