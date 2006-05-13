#ifndef _MODULE_H_
#define _MODULE_H_

typedef struct{
	int code_space;
	
	INT1 find_code_fun;		//The find code function.
	
	INT2 num_g_kinds;		//The number of global kinds listed.
	KindInd* g_kind_inds;	//The indexes of these global kinds.
	
	INT2 num_loc_kinds;		//The number of local kinds listed.
	LKind* loc_kinds;		//The local kinds.
	int kind_offset;			//The offset of local kind indexes.
	
	INT2 num_ty_skel;		//The number of type skeletons.
	TySkel* ty_skels;		//The type skeletons.
	int ty_skel_offset;		//Offset of type skeleton indexes.
	
	INT2 num_g_consts;		//The number of global constants
	ConstInd* g_const_inds;	//The indexes of these global kinds
	
	INT2 num_loc_consts;	//The number of local constants.
	LConst* loc_consts;	//The local constants.
	int loc_const_offset;		//The offset of local constant indexes.
	
	INT1 num_code_segs;
	
	INT1 num_acc_modules;	//The number of accumulated modules.
	Module* acc_modules;	//The descriptions of those modules.
	
	INT1 num_imp_modules;	//The number of imported modules.
	Module* imp_modules;	//The descriptions of those modules.
	
	INT2 num_strings;		//The number of strings.
	Name* strings;			//The strings.
	
	INT2 num_hid_consts;	//The number of hidden constants.
	Consts* hid_consts;		//The hidden constants.
	
	INT1 num_impl_goal;		//Number of implication goals
	ImplGoal* impl_goals;	//The info for those goals
	
	INT2 num_hash_tabs;	//Number of hash tables
	HashTab* hash_tabs;	//The hash tables
	
	INT2 num_bvar_tabs;	//The number of bound variable tables
	BvarTab* bvar_tabs;	//The tables
	
	INT2 num_ext_defs;		//The number of extending definitions
	ConstInd* ext_defs_ind;	//The indexes of these definitions
	
	INT2 num_preds;		//Number of predicates defined.
	int search_tab_size;		//Size of the search code table
	SearchTabEnt* search_tab;//The search table
	
	void* code;			//The code
}Module;

module* cur_mod; 		//The module currently being loaded

#endif //_MODULE_H_
