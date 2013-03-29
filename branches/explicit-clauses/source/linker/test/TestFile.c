#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"

void __CUT__unifinputcheck()
{
  EM_TRY
  {
    int inpipe[2];
    LK_FILE_xPipe(inpipe);
    
    LK_FILE_PUT1(inpipe[1],0xdc);
    LK_FILE_PUT1(inpipe[1],0xdc);
    ASSERT(0xdc==LK_FILE_GET1(inpipe[0]),"Byte get");
    ASSERT(0xdc==LK_FILE_GET1(inpipe[0]),"Byte get");
    LK_FILE_PUT2(inpipe[1],0xdcfb);
    ASSERT(0xdcfb==LK_FILE_GET2(inpipe[0]),"TwoBytes get");
    LK_FILE_PUT4(inpipe[1],0xfadbccbd);
    ASSERT(0xfadbccbd==LK_FILE_GET4(inpipe[0]),"4Bytes get");
    LK_FILE_PUTWord(inpipe[1],(Word)0x12345678);
    ASSERT(0x12345678==(int)LK_FILE_GETWord(inpipe[0]),"Word get");
    
    LK_FILE_PutString(inpipe[1],"foo");
    char* tmp=LK_FILE_GetString(inpipe[0]);
    ASSERT(!strcmp("foo",tmp),"String check");
    free(tmp);
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}