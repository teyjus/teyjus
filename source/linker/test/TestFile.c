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
    Name n;
    LK_FILE_GetName(inpipe[0],&n);
    ASSERT(3==n.size,"Size check");
    ASSERT(0==strcmp("foo",n.string),"String check");
    Clear(n);
    LK_FILE_Close(inpipe[0]);
    LK_FILE_Close(inpipe[1]);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__pipeinputcheck()
{
  EM_TRY
  {
    LK_FILE_PushPipeInput();
    
    LK_FILE_PipePUT1(0xdc);
    ASSERT(0xdc==GET1(),"Byte get");
    LK_FILE_PipePUT2(0xdcfb);
    ASSERT(0xdcfb==GET2(),"TwoBytes get");
    LK_FILE_PipePUT4(0xfadbccbd);
    ASSERT(0xfadbccbd==GET4(),"4Bytes get");
    LK_FILE_PipePUTWord((Word)0x12345678);
    ASSERT(0x12345678==(int)GETWord(),"Word get");
    LK_FILE_PipePutName("foo");
    Name n;
    GetName(&n);
    ASSERT(3==n.size,"Size check");
    ASSERT(0==strcmp("foo",n.string),"String check");
    Clear(n);
    PopInput();
    LK_FILE_PipeInputClose();
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}

void __CUT__pipeoutputcheck()
{
  EM_TRY
  {
    LK_FILE_PipeOutput();
    
    PUT1(0xdc);
    ASSERT(0xdc==LK_FILE_PipeGET1(),"Byte get");
    PUT2(0xdcfb);
    ASSERT(0xdcfb==LK_FILE_PipeGET2(),"TwoBytes get");
    PUT4(0xfadbccbd);
    ASSERT(0xfadbccbd==LK_FILE_PipeGET4(),"4Bytes get");
    PUTWord((Word)0x12345678);
    ASSERT(0x12345678==(int)LK_FILE_PipeGETWord(),"Word get");
    Name n1={3,"foo"};
    PutName(n1);
    Name n;
    LK_FILE_PipeGetName(&n);
    ASSERT(3==n.size,"Size check");
    ASSERT(0==strcmp("foo",n.string),"String check");
    Clear(n);
    CloseOutput();
    LK_FILE_PipeOutputClose();
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
