#ifndef _TYSKEL_H_
#define _TYSKEL_H_
#include "datatypes.h"
//TySkel Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LTySkel_t;*/

extern void InitTTySkels();
extern void LoadTySkels();
extern void WriteTySkels();
extern int TySkelCmp(TySkelInd a, TySkelInd b);

#endif //_TYSKEL_H_
