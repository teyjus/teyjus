#ifndef _HASHTAB_H_
#define _HASHTAB_H_

//HashTab Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LHashTab_t;*/

void InitTHashTabs();
void LoadHashTabs();
void WriteHashTabs();


/////////////////////////////////////////
//Utilities////////////////////////////
/////////////////////////////////////////

int GetHashTabInd();
int MergeHashTabs(int a, int b);

#endif //_HASHTAB_H_
