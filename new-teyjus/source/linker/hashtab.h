#ifndef _HASHTAB_H_
#define _HASHTAB_H_

#include "datatypes.h"

//Initialization, Loading and Writing Functions
//for Hash Tables.
void InitTHashTabs();
void LoadHashTabs();
void WriteHashTabs();

typedef struct Vector* HashTab_t;

/////////////////////////////////////////
//Utilities////////////////////////////
/////////////////////////////////////////

void LoadHashTab(HashTab_t* HashTab);
void WriteHashTab(HashTab_t* HashTab);

//Returns the index of the hash table entry with key x
//Returns -1 on failure
//HashTab must be a legitimate hash table.
int HashTabSearch(HashTab_t* HashTab, ConstInd x);
CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x);

//Makes a=a+b and b=0;  Returns size of combined table.
INT1 MergeHashTabs(HashTabInd a, HashTabInd b,INT1 n);
void MergeFindCodeTabs(HashTab_t* a, HashTab_t* b);

#endif //_HASHTAB_H_
