#ifndef _IMPLGOAL_H_
#define _IMPLGOAL_H_

typedef struct{
	INT2 num_ext_pred;
	Const_ind* indexes;
	INT1 find_code_fun;
	int tab_size;
	INT2 num_preds;
	Search_tab_ent* ent;
}ImplGoal;

void loadImpGoal();

#endif //_IMPLGOAL_H_