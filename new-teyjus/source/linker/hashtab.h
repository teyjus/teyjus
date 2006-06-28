#ifndef _HASHTAB_H_
#define _HASHTAB_H_

//Initialization, Loading and Writing Functions
//for Hash Tables.
void InitTHashTabs();
void LoadHashTabs();
void WriteHashTabs();


/////////////////////////////////////////
//Utilities////////////////////////////
/////////////////////////////////////////

void LoadHashTab(struct Vector* HashTab);
void WriteHashTab(struct Vector* HashTab);

//Returns the index of the hash table entry with key x
//Returns -1 on failure
//HashTab must be a legitimate hash table.
int HashTabSearch(struct Vector* HashTab, ConstInd x);

//Makes a=a+b and b=0;  Returns size of combined table.
INT1 MergeHashTabs(HashTabInd a, HashTabInd b,INT1 n);
void MergeFindCodeTabs(struct Vector* a, struct Vector* b);

#endif //_HASHTAB_H_
