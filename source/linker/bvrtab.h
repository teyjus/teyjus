#ifndef _BVRTAB_H_
#define _BVRTAB_H_

typedef struct{
	INT1 tab_size;
	INT1 num_entries;
	bvr_tab_ent* table;
}BvrTab;

typedef struct{
	INT1 index;
	int code_index;
}BvrTabEnt;

void loadBvrTab();

#endif //_BVRTAB_H_