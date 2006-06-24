#ifndef _DATATYPES_H_
#define _DATATYPES_H_

#define INT1 unsigned char
#define INT2 unsigned short
#define INT4 unsigned int

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

#define DEBUG(x) printf("%s\n",x)

typedef struct{
	int size;
	char* string;
}Name;

#define Clear(name) free(name.string)

typedef struct{
	INT1 gl_flag;
	INT2 index;
}ConstInd;

typedef struct{
	INT1 gl_flag;
	INT2 index;
}KindInd;

typedef INT2 TySkelInd;
typedef INT2 HashTabInd;
typedef INT2 StringSpaceInd;
typedef INT2 BvrTabInd;
typedef INT2 ImplGoalInd;
typedef int CodeInd;
typedef int ImportTabInd;

#endif //_DATATYPES_H_
