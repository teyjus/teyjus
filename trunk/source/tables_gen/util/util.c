#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "util.h"


/**************************************************************************/
/* Space allocation                                                       */
/**************************************************************************/
/* allocate space of n bytes*/
void* UTIL_malloc(size_t n)
{
    void* ptr = (void*)malloc(n);
    if (ptr) return ptr;
    printf("cannot allocate space\n");
    exit(1);
    
}


/* allocate space for a string of given size */
char* UTIL_mallocStr(size_t size)
{
    char* ptr = (char*)malloc(sizeof(char)*(size + 1));
    if (ptr) return ptr;

    printf("cannot allocate space\n");
    exit(1);
}


/**************************************************************************/
/* string operation                                                       */
/**************************************************************************/
/* Append two strings */
char* UTIL_appendStr(char* str1, char* str2)
{
  size_t length = strlen(str1) + strlen(str2);
  char* ptr = UTIL_mallocStr(length + 1);
  
  strcpy(ptr, str1);
  strcat(ptr, str2);

  return ptr;
}


//convert lower case letters in a string to upper case ones
char* UTIL_upperCase(char* str)
{
    char *newstr, *tmp;
    newstr = strdup(str);
    tmp = newstr;
    while ((*tmp) != '\0'){
        if ((97 <= (int)*tmp) && ((int)*tmp <= 122))
            *tmp = (char)((int)*tmp - 32);
        tmp++;
    }
    return newstr;
}

//convert to lower cases     
char* UTIL_lowerCase(char* str)
{
    char *newstr, *tmp;
    newstr = strdup(str);
    tmp = newstr;
    while ((*tmp) != '\0'){
        if ((65 <= (int)*tmp) && ((int)*tmp) <= 90)
            *tmp = (char)((int)*tmp + 32);
        tmp++;
    }
    return newstr;
}

//covert an non-negtive integer to string
char* UTIL_itoa(int num)
{
    char *str = UTIL_mallocStr(33);
    sprintf(str, "%d", num);
    return str;
}


/**************************************************************************/
/* file operation                                                         */
/**************************************************************************/

/* open file in read mode */
FILE* UTIL_fopenR(char* filename)
{
    FILE* filePtr = fopen(filename, "r");
    if (filePtr) return filePtr;
    
    printf("cannot open input file %s\n", filename);
    exit(1);
}


/* open file in write mode */
FILE* UTIL_fopenW(char* filename)
{
    FILE* filePtr = fopen(filename, "w");
    if (filePtr) return filePtr;
    
    printf("cannot open output file %s\n", filename);
    exit(1);
}

/* close file */
void UTIL_fclose(FILE* file)
{
    fclose(file);
}
