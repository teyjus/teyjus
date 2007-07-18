/****************************************************************************
 *                                                                          *
 * system/stream.h{c} implements stream support for all parts of the LP     *
 * system.                                                                  *
 ****************************************************************************/
#ifndef STREAM_H
#define STREAM_H

#include <stdarg.h>
#include <stdio.h>
#include "../simulator/mctypes.h" //to be modified

/*****************************************************************************
 *                                CONSTANTS                                  *
 *****************************************************************************/

#define STREAM_ILLEGAL    0

#define STREAM_READ       "r"
#define STREAM_WRITE      "w"
#define STREAM_APPEND     "a"

/*****************************************************************************
 *                             EXPORTED VARIABLES                            *
 *****************************************************************************/
/* STREAMs corresponding to the three standard streams */
extern WordPtr STREAM_stdin, STREAM_stdout, STREAM_stderr;

/****************************************************************************
 *                      BASIC FUNCTIONS                                     *
 ****************************************************************************/

/* STREAM_open returns STREAM_ILLEGAL if the stream can't be opened;
   if inDoCountLines is false, the line numbering calls below will not
   work. */
WordPtr  STREAM_open(char *inFilename, char *inMode, int inDoUsePaths);
/* open strings as streams.  Note that the STREAM system does not
   distinguish to_string and from_string streams after they are
   opened.  Results are undefined for a write to a from_string or read
   from a to_string. */
WordPtr  STREAM_fromString(char *inString, int inDoCopyString);
WordPtr  STREAM_toString();

/* will not close the standard			    streams */
void     STREAM_close(WordPtr inStream); 


/***************************************************************************
 *                    RAW I/O SUPPORT ROUTINES                             *
 *      each routine returns -1 to indicate an error                       * 
 ***************************************************************************/
int STREAM_readCharacters(WordPtr inStream, int inMaxCount, char* outString, 
                          int inDoStopOnNewline);
/* STREAM_printf returns the number of characters written */
int     STREAM_printf(WordPtr inStream, char *format, ...);
int     STREAM_lookahead(WordPtr inStream, char *outChar);
Boolean STREAM_eof(WordPtr inStream); 
int     STREAM_flush(WordPtr inStream);

/***************************************************************************
 *                             ACCESSORS                                   *  
 ***************************************************************************/
char*   STREAM_getString(WordPtr inStream);
FILE*   STREAM_getFile(WordPtr inStream);
char*   STREAM_getName(WordPtr inStream);

#endif //STREAM_H
