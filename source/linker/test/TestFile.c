#include <stdio.h>
#include <cut-2/cut.h>
#include <string.h>
#include "system/error.h"
#include "linker/datatypes.h"
#include "linker/file.h"

void __CUT__filecheck()
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
    LK_FILE_PipePUTWord(0x12345678);
    ASSERT(0x12345678==GETWord(),"Word get");
    Name n1={3,"foo"};
    LK_FILE_PipePutName(n1);
    Name n;
    GetName(&n);
    ASSERT(3==n.size,"Size check");
    ASSERT(0==strcmp("foo",n.string),"String check");
    Clear(n);
  }
  EM_CATCH
  {
    ASSERT(0,"????");
  }
}
