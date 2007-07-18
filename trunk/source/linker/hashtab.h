#ifndef _HASHTAB_H_
#define _HASHTAB_H_

#include "vector.h"

typedef struct Vector HashTab_t;

#include "datatypes.h"
#include "module.h"

//Initialization, Loading and Writing Functions
//for Hash Tables.
extern void InitTHashTabs();
extern void LoadHashTabs(int fd, struct Module_st* CMData);
extern void WriteHashTabs(int fd);

/////////////////////////////////////////
//Utilities////////////////////////////
/////////////////////////////////////////

extern void LoadHashTab(int fd, struct Module_st* CMData,void* entry);
extern void WriteHashTab(int fd, void* entry);

//Returns the index of the hash table entry with key x
//Returns -1 on failure
//HashTab must be a legitimate hash table.
extern int HashTabSearch(HashTab_t* HashTab, ConstInd x);
extern CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x);

//Makes a=a+b and b=0;  Returns size of combined table.
extern Byte MergeHashTabs(HashTabInd a, HashTabInd b,Byte n);
extern void MergeFindCodeTabs(HashTab_t* a, HashTab_t* b);

#endif //_HASHTAB_H_
