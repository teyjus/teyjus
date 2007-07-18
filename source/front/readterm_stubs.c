#include "../simulator/builtins/readterm.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"

void c_initLocalTabs(value numFvs, value numTyFvs, value numTermArgs, 
                     value numTypeArgs)
{
    CAMLparam4 (numFvs, numTyFvs, numTermArgs, numTypeArgs);
    RT_initLocalTabs(Int_val(numFvs), Int_val(numTyFvs),
                     Int_val(numTermArgs), Int_val(numTypeArgs));
    CAMLreturn0;
}


void c_cleanLocalTabs(value v)
{
  CAMLparam1 (v);
  RT_cleanLocalTabs();
  CAMLreturn0;
}


void c_buildFreeVariable(value name, value index)
{
    CAMLparam2 (name, index);
    RT_buildFreeVar(String_val(name), Int_val(index));
    CAMLreturn0;
}

void c_buildFreeTypeVariable(value index)
{
    CAMLparam1 (index);
    RT_buildFreeTypeVar(Int_val(index));
    CAMLreturn0;
}

void c_buildIntTerm(value i)
{
    CAMLparam1 (i);
    RT_buildIntTerm(Int_val(i));
    CAMLreturn0;
}

void c_buildRealTerm(value f)
{
    CAMLparam1 (f);
    RT_buildRealTerm(Double_val(f));
    CAMLreturn0;
}

void c_buildStringTerm(value s)
{
    CAMLparam1 (s);
    RT_buildStringTerm(String_val(s));
    CAMLreturn0;
}

void c_buildNilTerm(value v)
{
    CAMLparam1 (v);
    RT_buildNilTerm();
    CAMLreturn0;
}


void c_buildMConstantTerm(value index)
{
    CAMLparam1 (index);
    RT_buildMConstantTerm(Int_val(index));
    CAMLreturn0;
}

void c_buildPConstantTerm(value index, value tyEnvSize)
{
    CAMLparam2 (index, tyEnvSize);
    RT_buildPConstantTerm(Int_val(index), Int_val(tyEnvSize));
    CAMLreturn0;
}

void c_buildFreeVarTerm(value index)
{
    CAMLparam1 (index);
    RT_buildFreeVarTerm(Int_val(index));
    CAMLreturn0;
}

void c_buildDBTerm(value index)
{
    CAMLparam1 (index);
    RT_buildDBTerm(Int_val(index));
    CAMLreturn0;
}

void c_buildAbstractionTerm(value numAbs)
{
    CAMLparam1 (numAbs);
    RT_buildAbstractionTerm(Int_val(numAbs));
    CAMLreturn0;
}

void c_buildConsTerm(value v)
{
    CAMLparam1 (v);
    RT_buildConsTerm();
    CAMLreturn0;
}

void c_buildApplicationTerm(value arity)
{
    CAMLparam1 (arity);
    RT_buildApplicationTerm(Int_val(arity));
    CAMLreturn0;
}

void c_buildArrowType(value v)
{
  CAMLparam1 (v);
  RT_buildArrowType();
  CAMLreturn0;
}

void c_buildSortType(value index)
{
  CAMLparam1 (index);
  RT_buildSortType(index);
  CAMLreturn0;
}

void c_buildStrType(value index, value arity)
{
  CAMLparam2 (index, arity);
  RT_buildStrType(index, arity);
  CAMLreturn0;
}

void c_buildFreeVarType(value index)
{
  CAMLparam1 (index);
  RT_buildFreeVarType(index);
  CAMLreturn0;
}
