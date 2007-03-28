/**************************************************************************/
/* util.h{c}.                                                             */
/* Auxiliary functions needed for generating source files.                */
/**************************************************************************/
#ifndef UTIL_H
#define UTIL_H
#include <stdio.h>


/**************************************************************************/
/* Space allocation                                                       */
/**************************************************************************/
/* allocate space */
void* UTIL_malloc(int size);

/* allocate space for a string of given size */
char* UTIL_mallocStr(int size);

/**************************************************************************/
/* string operation                                                       */
/**************************************************************************/
/* append two strings */
char* UTIL_appendStr(char* str1, char* str2);
/* capitalizing       */
char* UTIL_upperCase(char* str);
/* to lower cases     */
char* UTIL_lowerCase(char* str);
/* covert a non-negative integer to string */
char* UTIL_itoa(int num);

/**************************************************************************/
/* file operation                                                         */
/**************************************************************************/
/* open file in read mode */
FILE* UTIL_fopenR(char* filename);

/* open file in write mode */
FILE* UTIL_fopenW(char* filename);

/* close file */
void UTIL_fclose(FILE* file);


/* bool type */
typedef enum {
    UTIL_FALSE, UTIL_TRUE
} UTIL_Bool;

#endif


