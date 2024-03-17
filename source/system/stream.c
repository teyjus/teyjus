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
 * system/stream.h{c} implements stream support for the C parts of the LP   *
 * system.                                                                  *
 ****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "stream.h"
#include "error.h"
#include "streamlocal.h"
//include "paths.h"


/*****************************************************************************
 *                                CONSTANTS                                  *
 *****************************************************************************/
#define STREAM_MAX_LINE_LENGTH 1024

/***************************************************************************/
/* tables: STREAM_vtable and STREAM_standardStreams                        */
/***************************************************************************/

/* the vtables for the streams. */
static STREAML_Vtable STREAM_vtable[6] = 
{
    {	/* InFile */			
        NULL, 
        STREAM_file_readCharacters, 
        STREAM_file_lookahead, 
        STREAM_file_flush,
        STREAM_file_eof,
        STREAM_file_close,
        NULL,
        STREAM_file_getFile,
        NULL,
        STREAM_file_getName
    },

    {  /* OutFile */
        STREAM_file_printf,
        NULL,
        NULL,
        STREAM_file_flush,
        NULL,
        STREAM_file_close,
        STREAM_file_remove,
        STREAM_file_getFile,
        NULL,
        STREAM_file_getName
    },

    {  /* InString */
        NULL,
        STREAM_string_readCharacters,
        STREAM_string_lookahead,
        STREAM_string_flush,
        STREAM_string_eof,
        STREAM_string_close,
        NULL,
        NULL,
        STREAM_string_getString,
        STREAM_string_getName
    },
    
    {  /* OutString */
        STREAM_string_printf,
        NULL,
        NULL,
        STREAM_string_flush,
        NULL,
        STREAM_string_close,
        NULL,
        NULL,
        STREAM_string_getString,
        STREAM_string_getName
    },
    
    {  /* StdinFile */
        NULL,
        STREAM_file_readCharacters,
        STREAM_file_lookahead,
        STREAM_file_flush,
        STREAM_file_eof,
        NULL,
        NULL,
        STREAM_file_getFile,
        NULL,
        STREAM_file_getName
    },
    
    {  /* StdoutFile */
        STREAM_file_printf,
        NULL,
        NULL,
        STREAM_file_flush,
        NULL,
        NULL,
        NULL,
        STREAM_file_getFile,
        NULL,
        STREAM_file_getName
    }
};


/* the three standard streams are allocated statically to eliminate
   the need for an init function */
static STREAML_Stream STREAM_standardStreams[3] =
{

   { &STREAM_standardStreams[1], NULL, STREAML_STDINFILE,
     &STREAM_vtable[STREAML_STDINFILE], { NULL, "std_in" } },

   { &STREAM_standardStreams[2], NULL, STREAML_STDOUTFILE,
     &STREAM_vtable[STREAML_STDOUTFILE], { NULL, "std_out" } },

   { NULL, NULL, STREAML_STDOUTFILE,
     &STREAM_vtable[STREAML_STDOUTFILE], { NULL, "std_err" } }

};

static STREAML_Stream *STREAM_firstStream = &STREAM_standardStreams[0];

/*****************************************************************************
 *                             EXPORTED VARIABLES                            *
 *****************************************************************************/

WordPtr STREAM_stdin  =  (WordPtr)&STREAM_standardStreams[0];
WordPtr STREAM_stdout =  (WordPtr)&STREAM_standardStreams[1];
WordPtr STREAM_stderr =  (WordPtr)&STREAM_standardStreams[2];


/*****************************************************************************
 *                       PRIVATE AUXILIARY FUNCTIONS                         *
 *****************************************************************************/
static void STREAM_listInsert(STREAML_Stream *inStream)
{
    inStream->mNext = STREAM_firstStream;
    STREAM_firstStream = inStream;
}

static void STREAM_listRemove(STREAML_Stream *inStream)
{
    STREAML_Stream *aStream = STREAM_firstStream;
    STREAML_Stream **lPointerToAStream = &STREAM_firstStream;
    
    while (aStream) {
        if (aStream == inStream){
            *lPointerToAStream = inStream->mNext;
            return;
        }

        lPointerToAStream = &aStream->mNext;
        aStream = aStream->mNext;
    }
}

static void STREAM_checkInit()
{
   /* Make sure initialization has been done. */

   if (!STREAM_standardStreams[0].mUnion.mFile.mFile)
   {
      /* These can be initialized statically on some platforms (std*
	 is a constant initializer), but not on others.  Thus, we do
	 it dynamically. */
      STREAM_standardStreams[0].mUnion.mFile.mFile = stdin;
      STREAM_standardStreams[1].mUnion.mFile.mFile = stdout;
      STREAM_standardStreams[2].mUnion.mFile.mFile = stderr;
   }
}


/******************************************************************************/
/*                  IMPLEMENTATIONS                                           */
/******************************************************************************/
/* The functions to which dispatching takes place. */
int STREAM_file_printf(STREAML_Stream *inStream, char *format, va_list ap)
{
    return vfprintf(inStream->mUnion.mFile.mFile, format, ap);
}

int STREAM_string_printf(STREAML_Stream *inStream, char *format, va_list ap)
{
    char lBuffer[STREAM_MAX_LINE_LENGTH];
    char *lSource, *lDestination;
    int lNewBytes;
    
    /* print into a string */
    lNewBytes = vsprintf(lBuffer, format, ap);

    /* if the new length of the string is greater than the size of
       the block we have */
    if (inStream->mUnion.mString.mOffset + lNewBytes >=
        inStream->mUnion.mString.mLength) {
        /* allocate twice the space we need to get something like
           amortized constant time */
        int lNewSize = inStream->mUnion.mString.mOffset + lNewBytes * 2;
        char *lNewPtr =(char *)
            EM_realloc((void *)inStream->mUnion.mString.mData, lNewSize);

        //if (!lNewPtr) return -1; //error should have been caught in EM_realloc

        inStream->mUnion.mString.mData = lNewPtr;
        inStream->mUnion.mString.mLength = lNewSize;
    }

    /* copy the string */
    lSource = lBuffer;
    lDestination = inStream->mUnion.mString.mData + 
        inStream->mUnion.mString.mOffset;
    while ((*(lDestination++) = *(lSource++)));

    inStream->mUnion.mString.mOffset += lNewBytes;
    return 0;
}

int STREAM_file_readCharacters(STREAML_Stream *inStream, int inMaxCount,
                               char *outString, int inDoStopOnNewline)
{
    if (inDoStopOnNewline) {
        if (fgets(outString, inMaxCount, inStream->mUnion.mFile.mFile) == NULL)
            return -1;
    } else {
        int num_read = fread(outString, 1, inMaxCount-1,
                             inStream->mUnion.mFile.mFile);
        outString[num_read] = '\0'; /* null-terminate the string */
        
        /* if nothing was read or we encountered an error, let the
           caller know */
        if (num_read == 0) return -1;
    }
    return 0;
}

int STREAM_string_readCharacters(STREAML_Stream *inStream, int inMaxCount,
                                 char *outString, int inDoStopOnNewline)
{
    char *lString = inStream->mUnion.mString.mData;
    int   lOffset = inStream->mUnion.mString.mOffset;
    int   lLength = inStream->mUnion.mString.mLength;

    /* if we're at the end of the string, return an error */
    if (lOffset >= lLength) return -1;

    if (inDoStopOnNewline) {
        /* predecrement means we'll save space for the null
           terminator, unless we hit a newline... */
        while (--inMaxCount && (*outString = lString[lOffset]) &&
               (*outString != '\n'))
            outString++, lOffset++;

        /* if we ended on a newline, put our null terminator after
           it if we have room */
        if (*outString == '\n' && inMaxCount) {
            *(++outString) = '\0';
            ++lOffset; /* eat the newline in mData */
        } else *outString = '\0';
    } else {
        /* predecrement means we'll save space for the null terminator */
      while (--inMaxCount && (*outString = lString[lOffset]))
          outString++, lOffset++;

      /* insert the null terminator */
      *outString = '\0';
    }
    
    /* put our local copy back into the object */
    inStream->mUnion.mString.mOffset = lOffset;
    
    return 0;
}

int STREAM_file_lookahead(STREAML_Stream *inStream, char *outChar)
{
    int c;
    c = getc(inStream->mUnion.mFile.mFile);
    if (c == EOF) return -1;

    *outChar = c;
    ungetc(c, inStream->mUnion.mFile.mFile); /* push it back into the stream */
    return 0;
}

int STREAM_string_lookahead(STREAML_Stream *inStream, char *outChar)
{
    *outChar = inStream->mUnion.mString.mData[inStream->mUnion.mString.mOffset];
    if (*outChar == '\0') return -1;
    return 0;
}

int STREAM_file_flush(STREAML_Stream *inStream)
{
   fflush(inStream->mUnion.mFile.mFile);
   return 0;
}

int STREAM_string_flush(STREAML_Stream *inStream)
{
   return 0; /* success */
}

int STREAM_file_eof(STREAML_Stream *inStream)
{
    int ch = fgetc(inStream->mUnion.mFile.mFile);
    if (ch == EOF) return 1;
    else { ungetc(ch, inStream->mUnion.mFile.mFile); return 0; }
    /*  return feof(inStream->mUnion.mFile.mFile); */
}

int STREAM_string_eof(STREAML_Stream *inStream)
{
    return !inStream->mUnion.mString.mData[inStream->mUnion.mString.mOffset];
}

int STREAM_file_close(STREAML_Stream *inStream)
{
    fclose(inStream->mUnion.mFile.mFile);
    STREAM_listRemove(inStream);
    return 0;
}

int STREAM_file_remove(STREAML_Stream *inStream)
{
    remove(inStream->mUnion.mFile.mName);
    STREAM_listRemove(inStream);
    return 0;
}

int STREAM_string_close(STREAML_Stream *inStream)
{
    if (inStream->mUnion.mString.mMalloc && inStream->mUnion.mString.mData)
        free(inStream->mUnion.mString.mData);
    STREAM_listRemove(inStream);
    return 0;
}

FILE *STREAM_file_getFile(STREAML_Stream *inStream)
{
    return inStream->mUnion.mFile.mFile;
}

char *STREAM_string_getString(STREAML_Stream *inStream)
{
    return inStream->mUnion.mString.mData;
}

char *STREAM_file_getName(STREAML_Stream *inStream)
{
    return inStream->mUnion.mFile.mName;
}

char *STREAM_string_getName(STREAML_Stream *inStream)
{
    return inStream->mUnion.mString.mData;
}

/*****************************************************************************/
/*                   STREAM OPERATIONS                                       */
/*****************************************************************************/
/*                      OPENING                                              */
WordPtr STREAM_open(char *inFilename, char *inMode, int inDoUsePaths)
{
    char *lFilename = NULL;
    FILE *lFile = NULL;
    //PATHS_Pathname *lPathname = NULL;
    STREAML_Stream *lStream = NULL;
    
    STREAM_checkInit();
    
    EM_TRY {
        lStream = (STREAML_Stream *)EM_malloc(sizeof(STREAML_Stream));
        
        if (inDoUsePaths) {/*
            lPathname = PATHS_makePathname(inFilename);
            if (!lPathname || !PATHS_search(lPathname)) EM_THROW(EM_FAIL);
            
            lFilename = EM_strdup(PATHS_getFullname(lPathname));
            PATHS_free(lPathname);
            lPathname = NULL;*/
        } else lFilename = EM_strdup(inFilename);
        
        lFile = fopen(lFilename, inMode);
        if (!lFile) EM_THROW(EM_FAIL);
        
        /* fill in the stream data structure */
        lStream -> mParserBuffer = NULL;
        lStream -> mType = strcmp(inMode, "r") ? STREAML_OUTFILE : 
            STREAML_INFILE;
        lStream -> mVtable = &STREAM_vtable[lStream -> mType];
        lStream -> mUnion.mFile.mFile = lFile;
        lStream -> mUnion.mFile.mName = lFilename;
        
        /* insert the stream into stream table */
        STREAM_listInsert(lStream);
    } EM_CATCH {
        //if (lPathname) PATH_free(lPathname);
        if (lFilename) free(lFilename);
        if (lFile)     fclose(lFile);
        if (lStream)   free(lStream);
        
        if (EM_CurrentExnType == EM_FAIL) return STREAM_ILLEGAL;
        else EM_RETHROW();
    }
    
    /* finally return it */
    return (WordPtr)lStream;
}

WordPtr  STREAM_fromString(char *inString, int inDoCopyString)
{
    STREAML_Stream *lStream;
    
    STREAM_checkInit();
    
    lStream = (STREAML_Stream *)EM_malloc(sizeof(STREAML_Stream));
    
    /* set up our data structure.  We strdup the string if requested. */
    lStream->mParserBuffer = NULL;
    lStream->mType = STREAML_INSTRING;
    lStream->mVtable = &STREAM_vtable[STREAML_INSTRING];
    lStream->mUnion.mString.mData = inDoCopyString ?
        EM_strdup(inString) : inString;
    lStream->mUnion.mString.mOffset = 0;
    lStream->mUnion.mString.mLength = strlen(inString);
    lStream->mUnion.mString.mMalloc = inDoCopyString;
    
    /* insert ... */
    STREAM_listInsert(lStream);

    /* and return our stream */
    return (WordPtr)lStream;
}

WordPtr STREAM_toString()
{
   STREAML_Stream *lStream;

   STREAM_checkInit();

   lStream = (STREAML_Stream *)EM_malloc(sizeof(STREAML_Stream));

   /* set up our data structure.  We strdup the string if requested. */
   lStream->mParserBuffer = NULL;
   lStream->mType = STREAML_OUTSTRING;
   lStream->mVtable = &STREAM_vtable[STREAML_OUTSTRING];
   lStream->mUnion.mString.mData = NULL;
   lStream->mUnion.mString.mOffset = 0;
   lStream->mUnion.mString.mLength = 0;
   lStream->mUnion.mString.mMalloc = TRUE;

   /* insert ... */
   STREAM_listInsert(lStream);

   /* and return our stream */
   return (WordPtr)lStream;
}

/*                                 CLOSING                                   */
void STREAM_close(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;
   if (lStream->mVtable->STREAML_close) 
       lStream->mVtable->STREAML_close(lStream);
}


/*                                   I/O                                     */

int STREAM_readCharacters(WordPtr inStream, int inMaxCount, char *outString, 
                          int inDoStopOnNewline)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_readCharacters)
      return lStream->mVtable->STREAML_readCharacters(lStream, inMaxCount,
                                                      outString, 
                                                      inDoStopOnNewline);
   else return -1;
}

int STREAM_printf(WordPtr inStream, char *format, ...)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;
   va_list ap;
   int result;

   STREAM_checkInit();

   va_start(ap, format);

   if (lStream->mVtable->STREAML_printf)
      result = lStream->mVtable->STREAML_printf(lStream, format, ap);
   else
      result = -1;

   va_end(ap);

   return result;
}

int STREAM_sans_printf(WordPtr outStream, char *str)
{ return STREAM_printf(outStream, "%s", str); }

int STREAM_lookahead(WordPtr inStream, char *outChar)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_lookahead)
      return lStream->mVtable->STREAML_lookahead(lStream, outChar);
   else return -1;
}

Boolean STREAM_eof(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_eof) 
       return lStream->mVtable->STREAML_eof(lStream);
   else return FALSE;
}

int STREAM_flush(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_flush)
      return lStream->mVtable->STREAML_flush(lStream);
   else return -1; //return 0 in Dustin's code ?
}

char* STREAM_getString(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_getString)
      return lStream->mVtable->STREAML_getString(lStream);
   else return 0;
}

FILE*   STREAM_getFile(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_getFile)
      return lStream->mVtable->STREAML_getFile(lStream);
   else return 0;
}
    
char*  STREAM_getName(WordPtr inStream)
{
   STREAML_Stream *lStream = (STREAML_Stream *)inStream;

   STREAM_checkInit();

   if (lStream->mVtable->STREAML_getName)
      return lStream->mVtable->STREAML_getName(lStream);
   else return 0;
}
