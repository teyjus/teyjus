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
 * system/streamlocal.h -- information shared between stream.c, but not     *
 * intended to be used publicly.                                            *
 *                                                                          *
 ****************************************************************************/

#ifndef STREAMLOCAL_H
#define STREAMLOCAL_H

#include <stdio.h>
#include <stdarg.h>

/*forward declaration */
struct STREAML_Stream;
typedef struct STREAML_Stream STREAML_Stream;


/*STREAML_Vtable : dispatch to various functions */
typedef struct STREAML_Vtable 
{
    int (*STREAML_printf)(STREAML_Stream *, char *, va_list);
    int (*STREAML_readCharacters)(STREAML_Stream *, int, char *, int);
    int (*STREAML_lookahead)(STREAML_Stream *, char *);
    int (*STREAML_flush)(STREAML_Stream *);
    int (*STREAML_eof)(STREAML_Stream *);
    int (*STREAML_close)(STREAML_Stream *);
    int (*STREAML_remove)(STREAML_Stream *);
    FILE *(*STREAML_getFile)(STREAML_Stream *);
    char *(*STREAML_getString)(STREAML_Stream *);
    char *(*STREAML_getName)(STREAML_Stream *);
} STREAML_Vtable;


/* functions to handle these actions in streams, implemented in stream.c */
int STREAM_file_printf(STREAML_Stream *, char *, va_list);
int STREAM_string_printf(STREAML_Stream *, char *, va_list);
int STREAM_file_readCharacters(STREAML_Stream *, int, char *, int);
int STREAM_string_readCharacters(STREAML_Stream *, int, char *, int);
int STREAM_file_lookahead(STREAML_Stream *, char *);
int STREAM_string_lookahead(STREAML_Stream *, char *);
int STREAM_file_flush(STREAML_Stream *);
int STREAM_string_flush(STREAML_Stream *);
int STREAM_file_eof(STREAML_Stream *);
int STREAM_string_eof(STREAML_Stream *);
int STREAM_file_close(STREAML_Stream *);
int STREAM_file_remove(STREAML_Stream *);
int STREAM_string_close(STREAML_Stream *);
FILE *STREAM_file_getFile(STREAML_Stream *);
char *STREAM_string_getString(STREAML_Stream *);
char *STREAM_file_getName(STREAML_Stream *);
char *STREAM_string_getName(STREAML_Stream *);

/* The various types of streams supported */
/* (Note: Parser related types are removed from Dustin's version) */
typedef enum STREAML_Type
{
   STREAML_INFILE = 0,
   STREAML_OUTFILE,
   STREAML_INSTRING,
   STREAML_OUTSTRING,
   STREAML_STDINFILE,		/* permanent */
   STREAML_STDOUTFILE 		/* permanent */
} STREAML_Type;


/* structure to contain our data for each stream */
struct STREAML_Stream 
{
    STREAML_Stream *mNext;
    void           *mParserBuffer;
    STREAML_Type    mType;
    STREAML_Vtable *mVtable;
    union { 
        struct {            //data for file stream
            FILE *mFile;
            char *mName;
        } mFile;
        struct {            //data for string stream
            char *mData;
            int   mOffset;
            int   mLength;
            int   mMalloc;
        } mString;
    } mUnion;
};

#endif //STREAMLOCAL_H
