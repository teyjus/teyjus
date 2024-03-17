//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
/****************************************************************************
 *                                                                          *
 *   File error.h -- error-handling functions                               *
 *                                                                          *
 ****************************************************************************/
#ifndef ERROR_C
#define ERROR_C

#include "error.h"
#include "tjsignal.h"
#include "message.h"
#include "../simulator/mctypes.h"
#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <stdarg.h>

Boolean EM_anyErrors;

/* Exception-stack handling.  See error.h for details. */

//function call environment stack
SIGNAL_jmp_buf *EM_ExnHandlerStack = NULL;
int EM_ExnHandlerStackTop = 0;
int EM_ExnHandlerStackSize = 0;

//exception type
EM_ExnType EM_CurrentExnType;



#define EM_NUM_GENERAL_ERROR_MESSAGES 5
static MSG_Msg EM_generalErrorMessages[EM_NUM_GENERAL_ERROR_MESSAGES] =
{
   { EM_OUT_OF_MEMORY, 0, "Cannot allocate memory.", EM_NEWLINE, EM_ABORT, 1 },
   { EM_OUT_OF_HEAP, 0, "Simulator heap exhausted.", EM_NEWLINE, EM_ABORT, 1 },
   { EM_NEWLINE, 0, "\n", 0, EM_EXIT, 1 },
   { EM_ERROR_COLON, 0, "Error: ", 0, EM_EXIT, 1 },
   { EM_WARNING_COLON, 0, "Warning: ", 0, EM_NO_EXN, 1 }
};

static MSG_MessageBlock EM_generalErrors =
{
   EM_NUM_GENERAL_ERROR_MESSAGES,
   1, EM_NUM_GENERAL_ERROR_MESSAGES,
   NULL,
   EM_generalErrorMessages
};

MSG_MessageBlock *MSG_messageBlockHead = &EM_generalErrors;

/****************************************************************************
 * The public functions                                                     *
 ****************************************************************************/

/****************************************************************************
 * Routines which will generate errors automatically.                       *
 ****************************************************************************/
void *EM_malloc(unsigned int len)
{
    void *p = malloc(len);
    if (!p) EM_error(EM_OUT_OF_MEMORY);
    return p;
}
void *EM_realloc(void *ptr, unsigned int len)
{
   void *p = realloc(ptr, len);
   if (!p)
      EM_error(EM_OUT_OF_MEMORY);
   return p;
}
char *EM_strdup(char *ptr)
{
   char *p = (char *)strdup(ptr);
   if (!p)
      EM_error(EM_OUT_OF_MEMORY);
   return p;
}
/****************************************************************************
 * The routine that gets called in the event of an error                    *
 ****************************************************************************/
void EM_error(int inIndex, ...)  
{
    va_list ap;
    int lExnType;

    /* get our arguments and print the message */
    va_start(ap, inIndex);
    lExnType = MSG_vMessage(inIndex, &ap);
    va_end(ap);
    /* decide what to do next; EM_Throw will call exit(1) if necessary
     */
    if (lExnType == EM_NO_EXN) return;
    else { /* note we've encountered an error */
        EM_anyErrors = 1;
        EM_THROW(lExnType);
    }
}

void EM_reset()
{
   EM_anyErrors = 0;
}

#endif //ERROR_C
