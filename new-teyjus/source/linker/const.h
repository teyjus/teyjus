#endif _CONST_H_
#define _CONST_H_

typedef struct{
	INT1 fixity;
	INT1 precedence;
	INT1 ty_env_size;
	INT1 ty_preserving_info;
	Name name;
	INT2 ty_skel_index;
}GConst;

typedef struct{
	INT1 fixity;
	INT1 precedence;
	INT1 ty_env_size;
	INT2 ty_skel_index;
}LConst;

typedef struct{
	INT1 gl_flag;
	INT2 index;
}ConstInd;

void loadTopGConsts();
void loadGConsts();
void loadLConsts();
void readConstInd();

#endif
