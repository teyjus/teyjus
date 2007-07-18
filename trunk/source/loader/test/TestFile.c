#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "loader/file.h"

void __CUT__filecheck( void )
{
  EM_TRY{
    LD_FILE_OpenPipe();
    LD_FILE_PipePUT1(10);
    ASSERT((int)LD_FILE_GET1()==10,"Byte get");
    LD_FILE_PipePUT2(20);
    ASSERT((int)LD_FILE_GET2()==20,"TwoByte get");
    //ASSERT(LD_FILE_GET4()==40,"FourByte get");
    LD_FILE_PipePUTWORD(80);
    ASSERT((int)LD_FILE_GETWORD()==80,"Word get");
    
    LD_FILE_PipePutString("somestring");
    ASSERT((int)LD_FILE_GET1()==10,"Byte get");
    char* buffer=EM_malloc(11);
    LD_FILE_GetString(buffer,10);
    ASSERT(0==strcmp(buffer,"somestring"),"String get");
  }EM_CATCH{
    ASSERT(0==1,"Error thrown");
  }
    
//   EM_TRY{
//     ASSERT(LD_FILE_Exists("test/testfile",".sample"),"File existence check");
//     LD_FILE_Open("test/testfile",".sample");
//     ASSERT((int)LD_FILE_GET1()==10,"Byte get");
//     ASSERT((int)LD_FILE_GET2()==20,"TwoByte get");
//     //ASSERT(LD_FILE_GET4()==40,"FourByte get");
//     ASSERT((int)LD_FILE_GETWORD()==80,"Word get");
//     char buffer[11];
//     LD_FILE_GetString(buffer,10);
//     
//     ASSERT(0==strcmp(buffer,"somestring"),"String get");
//   }EM_CATCH{
//     ASSERT(0==1,"Error thrown");
//   }
}

