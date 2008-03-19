#include "../linker/module.h"
#include "../linker/message.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"

value FRONT_link(value mod_str, value verb)
{
  CAMLparam2 (mod_str, verb);

  verbosity = Int_val(verb);
  
  EM_TRY
  {
    InitAll();
    LoadTopModule(String_val(mod_str));
    WriteAll(String_val(mod_str));
  }
  EM_CATCH
  {
    bad("Linking aborted due to an exception.\n");
    CAMLreturn(Val_int(1));
  }

  CAMLreturn(Val_int(0));
}
