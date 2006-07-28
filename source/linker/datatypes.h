#ifndef _DATATYPES_H_
#define _DATATYPES_H_

#include <sys/types.h>

#define INT1 u_int8_t
#define INT2 u_int16_t
#define INT4 u_int32_t
#define WORD int

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

#define DEBUG(x) printf("%s\n",x)
#define DEBUGNUM(x) printf("-%d-\n",x)

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
typedef unsigned int CodeInd;
typedef unsigned int ImportTabInd;

#endif //_DATATYPES_H_
