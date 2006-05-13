#ifndef _TOPLEVEL_H_
#define _TOPLEVEL_H_

//Top level module info
name mod_name;
nameList dep_mods;

int string_space=0;
int ty_skel_space=0;
int header_info_space=0;
int code_space=0;

int num_g_kinds=0;		//Number of global kinds
GKind* g_kinds=NULL;		//The global kinds

int num_l_kinds=0;		//The number of local kinds loaded so far.

int num_g_consts=0;		//Number of global constants
GConst* g_consts=NULL;		//The global constants
//End of top level mod info;

#endif //_TOPLEVEL_H_