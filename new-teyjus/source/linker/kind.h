#ifndef _KIND_H_
#define _KIND_H_

typedef struct{
	INT1 arity;
	name name;
}GKind;

typedef INT1 LKind;

typedef struct{
	INT1 gl_flag;
	INT2 index;
}KindInd;

void loadTopGKinds();
void loadGKinds();
void loadLKinds();
void readKindInd();

#endif //_KIND_H_