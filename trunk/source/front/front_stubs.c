#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"

#include "front.h"
#include "test.h"

void c_systemInit(value size)
{
    CAMLparam1 (size);
    FRONT_systemInit(Int_val(size));
    CAMLreturn0;
}

void c_simulatorInit(value v)
{
    CAMLparam1 (v);
    FRONT_simulatorInit();
    CAMLreturn0;
}

void c_topModuleInstall(value v)
{
    CAMLparam1 (v);
    FRONT_topModuleInstall();
    CAMLreturn0;
}

void c_moduleInstall(value ind)
{
    CAMLparam1 (ind);
    FRONT_moduleInstall(Int_val(ind));
    CAMLreturn0;
}


/* for testing */
void c_test(value v)
{
    CAMLparam1 (v);
    test();
    CAMLreturn0;
}
