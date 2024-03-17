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
/**************************************************************************/
/*                                                                        */
/* File builtin.c.                                                        */
/**************************************************************************/
#include <stdarg.h>
#include <errno.h>
#include <string.h>

#include "builtins.h"
#include "meta.h"
#include "evalexp.h"
#include "compexp.h"
#include "io.h"
#include "../mctypes.h"
#include "../dataformats.h"
#include "../printterm.h"
#include "../../tables/pervasives.h"
#include "../../system/error.h"
#include "../../system/message.h"
#include "../../system/stream.h"

/*****************************************************************/
/* Builtin Dispatch Table                                        */
/*****************************************************************/
/* Builtin Dispatch Table size */
#define BI_TAB_SIZE PERV_PREDEND - PERV_PREDSTART + 1

/* Builtin Dispatch Table */
static void (*BI_branchTab[BI_TAB_SIZE])() = 
{
    BIMETA_solve,                 //BI_SOLVE
    BIEVAL_eval,                  //BI_EVAL
    BIMETA_not,                   //BI_NOT
    BIMETA_unify,                 //BI_UNIFY

    BICOMP_comp,                  //BI_INT_LT
    BICOMP_comp,                  //BI_INT_GT        
    BICOMP_comp,                  //BI_INT_LE        
    BICOMP_comp,                  //BI_INT_GE        
    BICOMP_comp,                  //BI_FLOAT_LT 
    BICOMP_comp,                  //BI_FLOAT_GT    
    BICOMP_comp,                  //BI_FLOAT_LE      
    BICOMP_comp,                  //BI_FLOAT_GE      
    BICOMP_comp,                  //BI_STR_LT      
    BICOMP_comp,                  //BI_STR_GT         
    BICOMP_comp,                  //BI_STR_LE      
    BICOMP_comp,                  //BI_STR_GE

    BIIO_openIn,                  //BI_IO_OPEN_IN
    BIIO_openOut,                 //BI_IO_OPEN_OUT
    BIIO_openApp,                 //BI_IO_OPEN_APP
    BIIO_closeIn,                 //BI_IO_CLOSE_IN
    BIIO_closeOut,                //BI_IO_CLOSE_OUT
    BIIO_openStr,                 //BI_IO_OPEN_STR
    BIIO_input,                   //BI_IO_INPUT
    BIIO_output,                  //BI_IO_OUTPUT
    BIIO_inputLine,               //BI_IO_INPUT_LINE
    BIIO_lookahead,               //BI_IO_LOOKAHEAD  
    BIIO_eof,                     //BI_IO_EOF
    BIIO_flush,                   //BI_IO_FLUSH
    BIIO_print,                   //BI_IO_PRINT
    BIIO_read,                    //BI_IO_READ
    BIIO_printTerm,               //BI_IO_PRINTTERM
    BIIO_termToStr,               //BI_IO_TERMTOSTR 
    BIIO_strToTerm,               //BI_IO_STRTOTERM
    BIIO_readTerm,                //BI_IO_READTERM 
    BIIO_getEnv,                  //BI_IO_GETENV
    BIIO_openSocket,              //BI_IO_OPEN_SOCKET
    BIIO_unixTime,                //BI_IO_UNIX_TIME
    BIIO_system                   //BI_SYSTEM
};

/* builtin index "register" */
BI_BuiltinTabIndex BI_number;

void BI_dispatch(int index)
{
    BI_number = (BI_BuiltinTabIndex)index;
    (*BI_branchTab[index])();
}

/***************************######********************************************
 *                          Error Information
 *********************************######**************************************/


static MSG_Msg BI_errorMessages[BI_NUM_ERROR_MESSAGES] =
{
   { BI_ERROR,
     EM_ERROR_COLON,
     "%B: ",			/* Switch for the builtin's name */
     0, EM_NO_EXN, 0 },
   { BI_ERROR_TERM,
     0,
     "  %T",			/* Switch to print a term */
     EM_NEWLINE, EM_NO_EXN, 0 },
   { BI_ERROR_NOT_IMPLEMENTED,
     BI_ERROR,
     "Builtin %B is not implemented.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_FVAR_CAP,
     BI_ERROR,
     "Term free variable name table is full.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_TYFVAR_CAP,
     BI_ERROR,
     "Type free variable name table is full.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_DIV_BY_ZERO,
     BI_ERROR,
     "Division by zero.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_NEG_SQRT,
     BI_ERROR,
     "Square root with negative argument.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_NEG_LOG,
     BI_ERROR,
     "Logarithm with non-positive argument.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_CONST_IND,
     BI_ERROR,
     "Unexpected constant index %d.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_FLEX_HEAD,
     BI_ERROR,
     "Flexible term head in evaluation:",
     BI_ERROR_TERM, EM_QUERY, 0 },
   { BI_ERROR_ILLEGAL_ARG,
     BI_ERROR,
     "Illegal argument to evaluation:",
     BI_ERROR_TERM, EM_QUERY, 0 },
   { BI_ERROR_EVAL_TYPE,
     BI_ERROR,
     "Right side of is not of type int, real, or string.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_ILLEGAL_STREAM,
     BI_ERROR,
     "Illegal stream.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_FLEX_GOAL,
     BI_ERROR,
     "Ill-formed goal: uninstantiated variable as head.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_NON_VAR_TERM,
     BI_ERROR,
     "The term to be bound is not a variable:",
     BI_ERROR_TERM, EM_QUERY, 0 },
   { BI_ERROR_INDEX_OUT_OF_BOUNDS,
     BI_ERROR,
     "Index out of bounds.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_NEGATIVE_VALUE,
     BI_ERROR,
     "Positive value required: %d.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_UNBOUND_VARIABLE,
     BI_ERROR,
     "Expected %s, found unbound variable.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_NON_STREAM_TERM,
     BI_ERROR,
     "The term is not a stream:",
     BI_ERROR_TERM, EM_QUERY, 0 },
   { BI_ERROR_STREAM_ALREADY_CLOSED,
     BI_ERROR,
     "Attempting to close an input stream that is already closed.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_CANNOT_OPEN_STREAM,
     BI_ERROR,
     "Cannot open stream from `%s'.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_STREAM,
     0,
     " %T\n    %R",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_READING_STREAM,
     BI_ERROR,
     "Cannot read stream",
     BI_ERROR_STREAM, EM_QUERY, 0 },
   { BI_ERROR_WRITING_STREAM,
     BI_ERROR,
     "Cannot write stream",
     BI_ERROR_STREAM, EM_QUERY, 0 },
   { BI_ERROR_FLUSHING_STREAM,
     BI_ERROR,
     "Cannot flush stream",
     BI_ERROR_STREAM, EM_QUERY, 0 },
   { BI_ERROR_OPENING_STRING,
     BI_ERROR,
     "Cannot open string stream from `%s'",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_INTEGER_EXPECTED,
     BI_ERROR,
     "Integer expected:",
     BI_ERROR_TERM, EM_QUERY, 0 },
   { BI_ERROR_SUBSTRING,
     BI_ERROR,
     "Substring indexing out of bound.",
     EM_NEWLINE, EM_QUERY, 0 },
   { BI_ERROR_UNSET_ENV_VAR,
     BI_ERROR,
     "Environment variable `%s' is not set.",
     EM_NEWLINE, EM_QUERY, 0 },
};

/* handle the %B switch -- takes no arguments */
static void BI_printBuiltin(char *inSwitch, WordPtr inStream, 
                            va_list *ioArgument)
{
    STREAM_printf(inStream, "%s", 
                  PERV_constDataTab[BI_number+PERV_PREDSTART].name);
}

/* handle the %T switch -- takes one argument, a TERM_PTR_TY */
static void BI_printTerm(char *inSwitch, WordPtr inStream, va_list *ioArgument)
{
    int i;
    DF_TermPtr term = va_arg((*ioArgument), DF_TermPtr);
    
    PRINT_names = FALSE;
    PRINT_fPrintTerm(inStream, term);
    PRINT_resetPrintState();
}

/* handle the %R switch -- takes no arguments */
static void BI_printErrno(char *inSwitch, WordPtr inStream, va_list *ioArgument)
{
    char *lErrorMsg = NULL;
    if (errno)  lErrorMsg = strerror(errno);

    if (lErrorMsg) STREAM_printf(inStream, lErrorMsg);
    else STREAM_printf(STREAM_stderr, "(Not an OS error)");
}

/***************************######********************************************
 *                            Initialization
 *********************************######**************************************/

void BI_init()
{
    /* add our errors and switches */
    MSG_addMessages(BI_NUM_ERROR_MESSAGES, BI_errorMessages);
    MSG_addSwitch('B', BI_printBuiltin);
    MSG_addSwitch('T', BI_printTerm);
    MSG_addSwitch('R', BI_printErrno);
}
