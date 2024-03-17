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
 * builtins/io.c implements the i/o builtin routines.                       *
 *                                                                          *
 ****************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h> 
#include "builtins.h"
#include "readterm.h"
#include "../abstmachine.h" 
#include "../dataformats.h" 
#include "../hnorm.h"       
#include "../mcstring.h"    
#include "../mctypes.h"     
#include "../trail.h"       
#include "../printterm.h"   
#include "../types.h"
#include "../../tables/pervasives.h" 
#include "../../system/error.h"      
#include "../../system/stream.h"    
#include "../../front/readterm_c.h" 


/* unify types */
static void BIIO_typesUnify(DF_TypePtr typ1, DF_TypePtr typ2)
{
    AM_pdlError(2);
    AM_initTypesPDL();
    TY_pushPairsToPDL((MemPtr)typ1, (MemPtr)typ2, 1);
    TY_typesUnify();
}


/* get string from an lpwam string term pointer */
static char* BIIO_getStringFromTerm(DF_TermPtr tmPtr)
{
    HN_hnorm(tmPtr);
    tmPtr = DF_termDeref(tmPtr);
    if (DF_isStr(tmPtr)) return MCSTR_toCString(DF_strValue(tmPtr));
    else return NULL;
}

/* get stream index from an lpwam stream term */
static WordPtr BIIO_getStreamFromTerm(DF_TermPtr tmPtr)
{
    int cstInd;
    HN_hnorm(tmPtr);
    tmPtr = DF_termDeref(tmPtr);
    if (!DF_isStream(tmPtr)) {
        if (DF_isConst(tmPtr)) {
            cstInd = DF_constTabIndex(tmPtr);
            switch (cstInd) {
            case PERV_STDIN_INDEX:  return STREAM_stdin;
            case PERV_STDOUT_INDEX: return STREAM_stdout;
            case PERV_STDERR_INDEX: return STREAM_stderr;
            default: EM_error(BI_ERROR_NON_STREAM_TERM, tmPtr);
            }
        } else EM_error(BI_ERROR_NON_STREAM_TERM, tmPtr);
    } else {
        WordPtr stream = DF_streamTabIndex(tmPtr);
        if (stream == STREAM_ILLEGAL) EM_error(BI_ERROR_ILLEGAL_STREAM);
        else return stream;
    }

    //Impossible to reach this point.
    return NULL;
}

/* get value of an lpwam integer term pointer */
static int BIIO_getIntegerFromTerm(DF_TermPtr tmPtr)
{
    HN_hnorm(tmPtr);
    tmPtr = DF_termDeref(tmPtr);
    if (DF_isInt(tmPtr)) return DF_intValue(tmPtr);

    EM_error(BI_ERROR_INTEGER_EXPECTED);

    //Impossible to reach this point.
    return 0;
}

/* Given an lpwam VAR term pointer, and a stream index,
   bind the variable term to the given stream. */
static void BIIO_bindVarToStream(DF_TermPtr varPtr, WordPtr stream)
{   MemPtr nhreg;

    HN_hnorm(varPtr);
    varPtr = DF_termDeref(varPtr);
    if (!DF_isFV(varPtr)) EM_error(BI_ERROR_NON_VAR_TERM, varPtr);

    TR_trailTerm(varPtr);
    //GN, bug fix on June 20, 2012. 
    //Pointers to file descriptors have to be globalized. 
    //Otherwise, they could end up in registers (via put_value,
    //for example) and then updating them, such as when closing 
    //a stream, will update the contents of the register and 
    //not have the required global effect.
    nhreg = AM_hreg + DF_TM_ATOMIC_SIZE;
    AM_heapError(nhreg);
    DF_mkStream((MemPtr)AM_hreg,stream); 
    DF_mkRef((MemPtr)varPtr,(DF_TermPtr)AM_hreg); 
    AM_hreg = nhreg;
}

/* Given an lpwam VAR term pointer, and an integer value,
   bind the variable term to the given integer. */
static void BIIO_bindVarToInt(DF_TermPtr varPtr, int integer)
{
    HN_hnorm(varPtr);
    varPtr = DF_termDeref(varPtr);
    if (!DF_isFV(varPtr)) EM_error(BI_ERROR_NON_VAR_TERM, varPtr);

    TR_trailTerm(varPtr);
    DF_mkInt((MemPtr)varPtr, integer);
}

/* Given an lpwam VAR term pointer, and a C string,
   bind the variable term to the given string. */
static void BIIO_bindVarToString(DF_TermPtr varPtr, char* str)
{
    MemPtr strDataHead = AM_hreg;
    MemPtr strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr nhreg;
    int    length      = strlen(str);
    int    size        = MCSTR_numWords(length);

    nhreg = strData + size;
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, str, length);
    AM_hreg = nhreg;

    HN_hnorm(varPtr);
    varPtr = DF_termDeref(varPtr);
    if (!DF_isFV(varPtr)) EM_error(BI_ERROR_NON_VAR_TERM, varPtr);

    TR_trailTerm(varPtr);
    DF_mkStr((MemPtr)varPtr, (DF_StrDataPtr)strDataHead);
}

static void BIIO_closeStreamTerm(DF_TermPtr tmPtr)
{
    WordPtr stream;

    HN_hnorm(tmPtr);
    tmPtr = DF_termDeref(tmPtr);
    if (DF_isFV(tmPtr)) EM_error(BI_ERROR_UNBOUND_VARIABLE, "stream");
    
    if ((stream = DF_streamTabIndex(tmPtr)) == STREAM_ILLEGAL)
        EM_error(BI_ERROR_STREAM_ALREADY_CLOSED);

    STREAM_close(stream);
    DF_setStreamInd(tmPtr, STREAM_ILLEGAL);
}

static void BIIO_doOpen(char *inMode)
{
  WordPtr stream;
  char    *fname;

  fname = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));  
  if (!fname) EM_error(BI_ERROR_UNBOUND_VARIABLE, "filename");

  stream = STREAM_open(fname, inMode, FALSE);
  if (stream == STREAM_ILLEGAL) EM_error(BI_ERROR_CANNOT_OPEN_STREAM, fname);
  
  BIIO_bindVarToStream(((DF_TermPtr)AM_reg(2)), stream);
  AM_preg = AM_cpreg;
}

static void BIIO_doClose()
{
  BIIO_closeStreamTerm((DF_TermPtr)(AM_reg(1)));
  AM_preg = AM_cpreg;
}


/* type  open_in   string    -> in_stream -> o
         open_in   filename     stream
   opens file for reading                       */
void BIIO_openIn()       { BIIO_doOpen(STREAM_READ);   }

/* type  open_out, open_append   string -> out_stream -> o
         open_out, open_append   filename  strm
   opens file for writing. If the file does not exist,
   then a new file with indicated name is created, if possible.
   If the file already exists, then open_out truncates the file,
   and begins writing at the beginning, while open_append begins
   writing at the end.                          */
void BIIO_openOut()      { BIIO_doOpen(STREAM_WRITE);  }
void BIIO_openApp()      { BIIO_doOpen(STREAM_APPEND); }

/* type close_in    in_stream -> o
   close the stream                             */
void BIIO_closeIn()      { BIIO_doClose();             }
void BIIO_closeOut()     { BIIO_doClose();             }

/*  type open_string  string -> in_stream -> o
         open_string  str       X.
    creates an in_stream whose content is str and bind the
    in_stream to X                              */
void BIIO_openStr()
{
  char*    str;
  WordPtr  stream;
  
  str = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));
  if (!str) EM_error(BI_ERROR_UNBOUND_VARIABLE, "string");
  
  stream = STREAM_fromString(str, FALSE);
  if (stream == STREAM_ILLEGAL) EM_error(BI_ERROR_OPENING_STRING, str);
  
  BIIO_bindVarToStream((DF_TermPtr)AM_reg(2), stream); 
  AM_preg = AM_cpreg;
}

/* type  input    in_stream -> int -> string -> o
         input    strm         i      X.
   input at most i characters from strm. Block until i characters
   are available, or EOF is reached
 */
void BIIO_input()
{
    char*    buffer;
    int      num, length, size;
    WordPtr  stream;
    MemPtr   strDataHead = AM_hreg;
    MemPtr   strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr   nhreg;
    
    num = BIIO_getIntegerFromTerm((DF_TermPtr)AM_reg(2)) + 1; /* +1 for the null
                                                                 character */
    if (num <= 0) EM_error(BI_ERROR_NEGATIVE_VALUE, num);
    
    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    buffer = (char*)EM_malloc(num);
    if (STREAM_readCharacters(stream, num, buffer, FALSE) == -1)
        EM_error(BI_ERROR_READING_STREAM, (DF_TermPtr)AM_reg(1));

    length = strlen(buffer);
    size   = MCSTR_numWords(length);
    nhreg  = strData + size;

    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, buffer, length);
    free(buffer);
    AM_hreg = nhreg;

    DF_mkStr((MemPtr)AM_reg(1), (DF_StrDataPtr)strDataHead);
    DF_copyAtomic((DF_TermPtr)AM_reg(3), (MemPtr)AM_reg(2));
    AM_preg = AM_eqCode;
}

/* type    output   out_stream -> string -> o
           output   strm          str
   outputs str to the stream strm
 */
void BIIO_output()
{
  WordPtr stream;
  char*   str;
  
  stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
  str    = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(2));
  
  if (!str) EM_error(BI_ERROR_UNBOUND_VARIABLE, "string");
  
  if (STREAM_sans_printf(stream, str) == -1) 
      EM_error(BI_ERROR_WRITING_STREAM, (DF_TermPtr)AM_reg(1));
  
  AM_preg = AM_cpreg;
}


/* type      input_line    in_stream -> string -> o
             input_line    strm         X
   Blocks until a newline or EOF has been read from strm,
   and then return all characters up to and including that
   first newline or EOF
 */
void BIIO_inputLine()
{
    #define  MAX_LINE_LENGTH 1024
    char     buffer[MAX_LINE_LENGTH];
    WordPtr  stream;
    int      length, size;
    MemPtr   strDataHead = AM_hreg;
    MemPtr   strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr   nhreg;
    
    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    if (STREAM_readCharacters(stream, MAX_LINE_LENGTH, buffer, TRUE) == -1)
        EM_error(BI_ERROR_READING_STREAM, (DF_TermPtr)AM_reg(1));
    
    length = strlen(buffer);
    size   = MCSTR_numWords(length);
    nhreg  = strData + size;
    
    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, buffer, length);
    AM_hreg = nhreg;

    DF_mkStr((MemPtr)AM_reg(1), (DF_StrDataPtr)strDataHead);
    AM_preg = AM_eqCode;
}


/* type    lookahead    in_stream -> string -> o
           lookahead    strm         X.
   returns the next character from strm without removing it from the
   stream, or the empty string if at the end of file. Blocks until
   a character or EOF are available.
 */
void BIIO_lookahead()
{
    WordPtr  stream;
    char     c[2];
    int      length = 2;
    int      size   = MCSTR_numWords(length);
    MemPtr   strDataHead = AM_hreg;
    MemPtr   strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
    MemPtr   nhreg       = strData + size;

    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    if (STREAM_lookahead(stream, c) == -1) 
        EM_error(BI_ERROR_READING_STREAM, (DF_TermPtr)AM_reg(1));

    c[1] = '\0'; /* terminating the string */

    AM_heapError(nhreg);
    DF_mkStrDataHead(strDataHead);
    MCSTR_toString((MCSTR_Str)strData, c, length);
    AM_hreg = nhreg; 
    
    DF_mkStr((MemPtr)AM_reg(1), (DF_StrDataPtr)strDataHead);
    AM_preg = AM_eqCode;
}

/* type    eof     in_stream -> o
           eof     strm.
   succeeds if at the end of stream of strm
 */
void BIIO_eof()
{
    WordPtr stream;
    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    if (!STREAM_eof(stream)) EM_THROW(EM_FAIL); /* not eof -> fail */
    
    AM_preg = AM_cpreg;
}

/* type   flush   out_stream -> o
          flush   strm
   writes any buffered output to strm
 */
void BIIO_flush()
{
    WordPtr stream;
    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    if (STREAM_flush(stream) == -1) 
        EM_error(BI_ERROR_FLUSHING_STREAM, (DF_TermPtr)AM_reg(1));
    
    AM_preg = AM_cpreg;
}

/* type  print    string -> o
         print    X :- output std_out X.
   print a lpwam string term to standard output
 */
void BIIO_print()
{
    char *str = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));
    if (str) STREAM_sans_printf(STREAM_stdout, str);
    else EM_error(BI_ERROR_UNBOUND_VARIABLE, "string");
    
    AM_preg = AM_cpreg;
}


/*  type   printterm    out_stream -> A -> o
           printterm    strm X.
   prints the lpwam term X to the stream strm.
   The main part of the code is in Fpterm(), which is a function
   defined in: simulator/printterm.c
 */
void BIIO_printTerm()
{
    WordPtr stream;
    stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
    
    PRINT_names = FALSE;
    PRINT_fPrintTerm(stream, (DF_TermPtr)AM_reg(2));
    // NG - resetPrintState should only be called when a new query is made
    /* PRINT_resetPrintState(); */
    
    AM_preg = AM_cpreg;
}


/*  type  string_to_term    string -> A -> o
          string_to_term    str  X.
    parse the period-terminated string str to obtain an lpwam term
    and bind it to X */
void BIIO_strToTerm()
{   

  WordPtr     stream;
  char       *str;
  DF_TermPtr  tp;
  DF_TypePtr  typ;

  str = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));
  if (!str)
    EM_error(BI_ERROR_UNBOUND_VARIABLE);
    
  typ  = (DF_TypePtr)(AM_hreg);
  RT_setTypeStart(AM_hreg);
  AM_hreg += DF_TY_ATOMIC_SIZE;
    
  tp   = (DF_TermPtr)(AM_hreg);  
  RT_setTermStart(AM_hreg);
  AM_hreg += DF_TM_ATOMIC_SIZE;
  
  if (FRONT_RT_readTermAndType(str)) {
    PRINT_resetFreeVarTab();
  } else {
    EM_THROW(EM_FAIL);
  }
  
  
  BIIO_typesUnify(typ, (DF_TypePtr)(AM_reg(3)));
  
  DF_mkRef((MemPtr)AM_reg(1), tp);
  AM_preg = AM_eqCode;    

}

/*  type  readterm  in_stream -> A -> o
          readterm  strm X.
    read in a newline-terminated string from strm, parse it to get
    an lpwam term and unify it with X. */
void BIIO_readTerm()
{
#define  MAX_LINE_LENGTH 1024
  char        buffer[MAX_LINE_LENGTH];
  WordPtr     stream;

  DF_TermPtr  tp;
  DF_TypePtr  typ;
    
  stream = BIIO_getStreamFromTerm((DF_TermPtr)AM_reg(1));
  if (STREAM_readCharacters(stream, MAX_LINE_LENGTH, buffer, TRUE) == -1)
    EM_error(BI_ERROR_READING_STREAM);


  typ  = (DF_TypePtr)(AM_hreg);
  RT_setTypeStart(AM_hreg);
  AM_hreg += DF_TY_ATOMIC_SIZE;

  tp   = (DF_TermPtr)(AM_hreg);  
  RT_setTermStart(AM_hreg);
  AM_hreg += DF_TM_ATOMIC_SIZE;
  
  if (FRONT_RT_readTermAndType(buffer)) {
    PRINT_resetFreeVarTab();
  } else {
    EM_THROW(EM_FAIL);
  }
  

  BIIO_typesUnify(typ, (DF_TypePtr)(AM_reg(3)));
  
  DF_mkRef((MemPtr)AM_reg(1), tp);
  AM_preg = AM_eqCode;      
}


/*  type read   A -> o
         read   Y  :- input_line std_in X, string_to_term X Y.
    Universal reading. Read in a term from standard input.
    This is precisely what we have been calling "readterm" */
void BIIO_read() 
{
#define  MAX_LINE_LENGTH 1024
  char        buffer[MAX_LINE_LENGTH];
  WordPtr     stream = STREAM_stdin;
  DF_TermPtr  tp;
  DF_TypePtr  typ;   
    
  if (STREAM_readCharacters(stream, MAX_LINE_LENGTH, buffer, TRUE) == -1)
    EM_error(BI_ERROR_READING_STREAM);
    

  typ  = (DF_TypePtr)(AM_hreg);
  RT_setTypeStart(AM_hreg);
  AM_hreg += DF_TY_ATOMIC_SIZE;

  tp   = (DF_TermPtr)(AM_hreg);  
  RT_setTermStart(AM_hreg);
  AM_hreg += DF_TM_ATOMIC_SIZE;


  if (FRONT_RT_readTermAndType(buffer)) {
    PRINT_resetFreeVarTab();
  } else {
    EM_THROW(EM_FAIL);
  }
  

  BIIO_typesUnify(typ, (DF_TypePtr)(AM_reg(2)));
  
  DF_mkRef((MemPtr)AM_reg(2), tp);
  AM_preg = AM_eqCode;  
  
}



/*  type  term_to_string  A -> string -> o
          term_to_string  term X.
    convert the term into a string form and bind the string to X
 */
void BIIO_termToStr()
{
    WordPtr stream;
    char    *str;
    
    stream = STREAM_toString();
    
    PRINT_names = FALSE;
    PRINT_fPrintTerm(stream, (DF_TermPtr)AM_reg(1));
    // NG - resetPrintState should only be called when a new query is made
    /* PRINT_resetPrintState(); */
    
    /* if this fails, we depend upon the top level to close the
       stream..  The other option is to intercept failure after
       STREAM_toString, and call STREAM_close before passing on the
       failure. */
    str = STREAM_getString(stream);
    if (str) BIIO_bindVarToString((DF_TermPtr)AM_reg(2), str);
    else BIIO_bindVarToString((DF_TermPtr)AM_reg(2), "");
    
    STREAM_close(stream);
    AM_preg = AM_cpreg;
}

/* type getenv string -> string -> o.
        getenv Name Value.
   Calls the Unix getenv function to unify Value with the value of
   the environment variable given in Name. Name must be instantiated.
 */
void BIIO_getEnv()
{
  //NOTE: os dependent; need to add code for other os besides UNIX.
  char     *str, *envstr;
  int      length, size;
  MemPtr   strDataHead = AM_hreg;
  MemPtr   strData     = strDataHead + DF_STRDATA_HEAD_SIZE;
  MemPtr   nhreg;

  str = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));
  if (!str) EM_error(BI_ERROR_UNBOUND_VARIABLE, "string");

  envstr = getenv(str);
  if (envstr == NULL) EM_error(BI_ERROR_UNSET_ENV_VAR, str);

  length = strlen(envstr);
  size   = MCSTR_numWords(length);
  nhreg  = strData + size;
  
  AM_heapError(nhreg);
  DF_mkStrDataHead(strDataHead);
  MCSTR_toString((MCSTR_Str)strData, envstr, length);
  AM_hreg = nhreg;   
 
  DF_mkStr((MemPtr)AM_reg(1), (DF_StrDataPtr)strDataHead);
  AM_preg = AM_eqCode;
  return;
}

/* type open_socket string -> int -> in_stream -> out_stream -> o.
        open_socket INet PortNo  In  Out.
   INet must be instantiated to an inet address (in string form) and
   PortNo must be bound to a port number for a relevant service. In and
   Out are bound to streams associated with the socket so specified.
   Unix specific functions are used in the realization---see the
   definition of SSTREAM_open in the file /system/stream.c. In and Out
   are expected to be unbound whereas INet and PortNo must be fully
   instantiated.
*/
void BIIO_openSocket()
{
  EM_error(BI_ERROR_NOT_IMPLEMENTED);
}

/* type  time  int -> int -> o
   time X Y
   Uses the Unix gettimeofday function to get the number of seconds and
   microseconds in X and Y since 00:00 Universal Coordinated Time,
   January 1, 1970. The arguments are expected to be uninstantiated
   variables
*/
void BIIO_unixTime()
{
  //to be filled in
  EM_error(BI_ERROR_NOT_IMPLEMENTED);
}


/* type  system  string -> int -> o
   system Command ReturnCode.
*/
void BIIO_system()
{
  char * command = NULL;
  int result = -1;

  //Grab the command; it must be bound.
  command = BIIO_getStringFromTerm((DF_TermPtr)AM_reg(1));
  if (!command) EM_error(BI_ERROR_UNBOUND_VARIABLE, "string");

  //Execute
  result = system(command);

  //Store result.
  BIIO_bindVarToInt((DF_TermPtr)AM_reg(2), result);

  AM_preg = AM_cpreg;
  return;
}
