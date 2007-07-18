#ifndef _IMPLGOAL_H_
#define _IMPLGOAL_H_

//ImplGoal Header Data
//Note: No individual representation in CM.
/*typedef struct{
	//Empty
}LImplGoal_t;*/

extern void InitTImplGoals();
extern void LoadImplGoals(int fd, struct Module_st* CMData);
extern void WriteImplGoals(int fd);

#endif //_IMPLGOAL_H_
