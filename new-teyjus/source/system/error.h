/****************************************************************************
 *                                                                          *
 *   File error.h -- error-handling functions for all Teyjus builds.        *
 *   (TEMP)                                                                 *
 *                                                                          *
 ****************************************************************************/

#ifndef ERROR_H
#define ERROR_H

#include <stdlib.h>
#include <setjmp.h>
#include "tjsignal.h"

/****************************************************************************
 * Exception stack declarations.                                            *
 ****************************************************************************/

typedef enum EM_ExnType{
    EM_NO_EXN = 0,     // used for warnings
    EM_ABORT,          // exit the executable immediately
    EM_EXIT,           // traverse the exception stack and exit
    EM_TY_UNI_FAIL,    // fail from type unification
    EM_HOPU_FAIL       // fail from pattern unification
} EM_ExnType;


extern SIGNAL_jmp_buf *EM_ExnHandlerStack;
extern int             EM_ExnHandlerStackTop;
extern int             EM_ExnHandlerStackSize;
extern EM_ExnType      EM_CurrentExnType;

/****************************************************************************
 * Exception-handling macros                                                *
 ****************************************************************************/
#define EM_try \
if (EM_ExnHandlerStackTop >= EM_ExnHandlerStackSize) \
{ \
   EM_ExnHandlerStackSize = \
     (EM_ExnHandlerStackSize + 1) * 2; \
   EM_ExnHandlerStack = \
     (SIGNAL_jmp_buf *)EM_realloc((void *)EM_ExnHandlerStack, \
	   EM_ExnHandlerStackSize * sizeof(SIGNAL_jmp_buf)); \
} \
if (SIGNAL_setjmp(EM_ExnHandlerStack[EM_ExnHandlerStackTop++]) == 0) \
{

#define EM_catch \
   EM_ExnHandlerStackTop--; \
} \
else

/* pass the current exception to the next handler.  Use only within an
   EM_Catch block. */
#define EM_reThrow() \
   SIGNAL_longjmp(EM_ExnHandlerStack[--EM_ExnHandlerStackTop], 1)

/* Jump to the nearest (in a dynamic sense) EM_Try block, setting
   EM_CurrentExnType to TYPE. Given a constant, the conditional in
   this macro will be optimized away. */
#define EM_throw(type) EM_throwVal((type), 1)

#define EM_throwVal(type, val) \
do { \
   if ((type) == EM_ABORT) \
      exit(1); \
   else \
   { \
      EM_CurrentExnType = (type); \
      SIGNAL_longjmp(EM_ExnHandlerStack[--EM_ExnHandlerStackTop], val); \
   } \
} while(0)


/* Here's an example use of the above macros:

...
EM_Try
{
   foo();
   if (foobar)
      EM_Throw(EM_FOOBAR);
}
EM_Catch
{
   un_foo();			/* clean up *
   if (EM_CurrentExnType == EM_FOOBAR)
      printf("foobar!");        /* stop the error here *
   else
      EM_ReThrow();             /* let a later handler handle it *
}
*/

/****************************************************************************
 * Routines which will generate errors automatically.                       *
 ****************************************************************************/

void *EM_malloc(unsigned int);
void *EM_realloc(void *, unsigned int);

#endif  //ERROR_H 
