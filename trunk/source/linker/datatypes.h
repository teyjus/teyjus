#ifndef _DATATYPES_H_
#define _DATATYPES_H_

#include <sys/types.h>
#include "../simulator/mctypes.h"
#include "../system/error.h"

//#define Byte u_int8_t
//#define TwoBytes u_int16_t
#define INT4 u_int32_t
//#define Word int

#define GLOBAL 0
#define LOCAL 1
#define HIDDEN 2
#define PERVASIVE 3

//TySkel symbols
#define ARROW 0
#define KIND 1
#define VARIABLE 2

#define DEBUG(x) printf("%s\n",x)
#define DEBUGNUM(x) printf("-%d-\n",x)

typedef struct{
  int size;
  char* string;
}Name;

#define Clear(name) free(name.string)

typedef struct{
  Byte gl_flag;
  TwoBytes index;
}MarkInd;

typedef MarkInd ConstInd;
typedef MarkInd KindInd;

typedef TwoBytes TySkelInd;
typedef TwoBytes HashTabInd;
typedef TwoBytes StringInd;
typedef TwoBytes BvrTabInd;
typedef TwoBytes ImplGoalInd;
typedef long CodeInd;
typedef int ImportTabInd;

typedef enum {
  LK_LinkError=LOADER_FIRST_ERR_INDEX
}LK_ExnType;

#endif //_DATATYPES_H_
