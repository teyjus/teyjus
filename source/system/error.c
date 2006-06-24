/****************************************************************************
 *                                                                          *
 *   File error.h -- error-handling functions for all Teyjus builds.        *
 *   (TEMP)                                                                 *
 *                                                                          *
 ****************************************************************************/
#ifndef ERROR_C
#define ERROR_C

#include "error.h"
#include "tjsignal.h"
#include <stdlib.h>
#include <stdio.h> 

/* Exception-stack handling.  See error.h for details. */
SIGNAL_jmp_buf *EM_ExnHandlerStack = NULL;
int EM_ExnHandlerStackTop = 0;
int EM_ExnHandlerStackSize = 0;
EM_ExnType EM_CurrentExnType;

/****************************************************************************
 * The public functions                                                     *
 ****************************************************************************/

void EM_error()  //temp
{
    printf("\nEM_error\n");
    exit(1);
}


void *EM_malloc(unsigned int len)
{
    void *p = malloc(len);
    if (!p) EM_error();
    return p;
}


void *EM_realloc(void *ptr, unsigned int len)
{
   void *p = realloc(ptr, len);
   if (!p)
      EM_error();
   return p;
}


#endif //ERROR_C
