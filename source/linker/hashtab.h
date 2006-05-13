#ifndef _HASHTAB_H_
#define _HASHTAB_H_

typedef struct{
	INT1 tab_size;
	INT1 num_entries;
	HashTabEnt* table;
}HashTab;

typedef struct{
	Const_ind index;
	int code_index;
}Hash_tab_ent;

void loadHashTab();

#endif //_HASHTAB_H_