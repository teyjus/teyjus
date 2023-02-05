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
#include <stdio.h>
#include "../simulator/builtins/readterm.h"
#include "front_c.h"
#include "query_c.h"
#include "../loader/file.h"

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"


/***************************************************************************/
/*                          front                                          */
/***************************************************************************/
int c_systemInit(value size)
{
    CAMLparam1 (size);
    CAMLreturn (Val_int(FRONT_systemInit(Int_val(size))));
}

int c_simulatorInit(value v)
{
    CAMLparam1 (v);
    CAMLreturn (Val_int(FRONT_simulatorInit()));
}

int c_simulatorReInit(value imp)
{
    CAMLparam1 (imp);
    CAMLreturn (Val_int(FRONT_simulatorReInit(Bool_val(imp))));
}

int c_link(value modName)
{
    CAMLparam1 (modName);
    CAMLreturn (Val_int(FRONT_link(String_val(modName))));
}

int c_load(value modName, value index)
{
    CAMLparam2 (modName, index);
    CAMLreturn (Val_int(FRONT_load(String_val(modName), Int_val(index))));
}

int c_topModuleInstall(value v)
{
    CAMLparam1 (v);
    CAMLreturn (Val_int(FRONT_topModuleInstall()));
}

int c_moduleInstall(value ind)
{
    CAMLparam1 (ind);
    CAMLreturn (Val_int(FRONT_moduleInstall(Int_val(ind))));
}

int c_initModuleContext(value v)
{
    CAMLparam1 (v);
    CAMLreturn (Val_int(FRONT_initModuleContext()));
}

void c_cleanModule(value v)
{
    CAMLparam1 (v);
    FRONT_cleanModule();
    CAMLreturn0;
}

int c_setPath(value v)
{
    CAMLparam1 (v);
    CAMLreturn (Val_int(FRONT_setPath(String_val(v))));
}


/***************************************************************************/
/*                               query                                     */
/***************************************************************************/
// NG: No longer in use, since queries are now compiled
/* void c_setTypeAndTermLocation(value v) */
/* { */
/*     CAMLparam1 (v); */
/*     QUERY_setTypeAndTermLocation(); */
/*     CAMLreturn0; */
/* } */

int c_solveQuery(value v)
{
    int i;
    CAMLparam1 (v);
    i = QUERY_solveQuery();
    fflush(stdout);
    CAMLreturn (Val_int(i));
}

int c_showAnswers(value v)
{
    CAMLparam1 (v);
    CAMLreturn (Val_int(QUERY_showAnswers()));
}

void c_setQueryFreeVariables(value v)
{
  CAMLparam1 (v);
  QUERY_setQueryFreeVariables(Int_val(v));
  CAMLreturn0;
}

Boolean c_queryHasVars(value v)
{
    CAMLparam1 (v);
    if (QUERY_queryHasVars()) CAMLreturn(Val_int(1));
    else CAMLreturn(Val_int(0));
}

void c_loadQuery(value modName)
{
  CAMLparam1 (modName);
  QUERY_loadQuery(String_val(modName));
  CAMLreturn0;
}

void c_setQueryEntryPoint(value loc)
{
  CAMLparam1 (loc);
  QUERY_setQueryEntryPoint(Int_val(loc));
  CAMLreturn0;
}

/***************************************************************************/
/*                               pipes                                     */
/***************************************************************************/
void c_openPipe()
{
  CAMLparam0();
  LD_FILE_OpenPipe();
  CAMLreturn0;
}

int c_getPipeIn()
{
  CAMLparam0();
  int fd = LD_FILE_GetPipeIn();
  CAMLreturn(Val_int(fd));
}

/* void c_setInChannel(value chan) */
/* { */
/*   CAMLparam1(chan); */
/*   LD_FILE_SetInChannel(Int_val(chan)); */
/*   CAMLreturn0; */
/* } */

/***************************************************************************/
/*                               read term                                 */
/***************************************************************************/
// NG: No longer in use, since queries are now compiled
//     -> use initLocalTabsQuery instead
/* int c_initLocalTabs(value numFvs, value numTyFvs, value numTermArgs, */
/*                      value numTypeArgs) */
/* { */
/*     CAMLparam4 (numFvs, numTyFvs, numTermArgs, numTypeArgs); */
/*     CAMLreturn(Val_int(RT_initLocalTabs(Int_val(numFvs), Int_val(numTyFvs), */
/*                                         Int_val(numTermArgs), */
/*                                         Int_val(numTypeArgs)))); */
/* } */

int c_initLocalTabsQuery(value numFvs)
{
    CAMLparam1 (numFvs);
    CAMLreturn(Val_int(RT_initLocalTabsQuery(Int_val(numFvs))));
}


void c_cleanLocalTabs(value v)
{
  CAMLparam1 (v);
  RT_cleanLocalTabs();
  CAMLreturn0;
}


int c_buildFreeVariable(value name, value index)
{
    CAMLparam2 (name, index);
    CAMLreturn(Val_int(RT_buildFreeVar(String_val(name), Int_val(index))));
}

int c_buildFreeTypeVariable(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildFreeTypeVar(Int_val(index))));
}

int c_buildIntTerm(value i)
{
    CAMLparam1 (i);
    CAMLreturn(Val_int(RT_buildIntTerm(Int_val(i))));
}

int c_buildRealTerm(value f)
{
    CAMLparam1 (f);
    CAMLreturn(Val_int(RT_buildRealTerm(Double_val(f))));
}

int c_buildStringTerm(value s)
{
    CAMLparam1 (s);
    CAMLreturn(Val_int(RT_buildStringTerm(String_val(s))));
}

int c_buildNilTerm(value v)
{
    CAMLparam1 (v);
    CAMLreturn(Val_int(RT_buildNilTerm()));
}



int c_buildMConstantTerm(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildMConstantTerm(Int_val(index))));
}

int c_buildPConstantTerm(value index, value tyEnvSize)
{
    CAMLparam2 (index, tyEnvSize);
    CAMLreturn(Val_int(RT_buildPConstantTerm(Int_val(index), 
                                             Int_val(tyEnvSize))));
}

int c_buildFreeVarTerm(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildFreeVarTerm(Int_val(index))));
}

int c_buildDBTerm(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildDBTerm(Int_val(index))));
}

int c_buildAbstractionTerm(value numAbs)
{
    CAMLparam1 (numAbs);
    CAMLreturn(Val_int(RT_buildAbstractionTerm(Int_val(numAbs))));
}

int c_buildConsTerm(value v)
{
    CAMLparam1 (v);
    CAMLreturn(Val_int(RT_buildConsTerm()));
}

int c_buildApplicationTerm(value arity)
{
    CAMLparam1 (arity);
    CAMLreturn(Val_int(RT_buildApplicationTerm(Int_val(arity))));
}

int c_buildArrowType(value v)
{
    CAMLparam1 (v);
    CAMLreturn(Val_int(RT_buildArrowType()));
}

int c_buildSortType(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildSortType(Int_val(index))));
}

int c_buildStrType(value index, value arity)
{
    CAMLparam2 (index, arity);
    CAMLreturn(Val_int(RT_buildStrType(Int_val(index), Int_val(arity))));
}

int c_buildFreeVarType(value index)
{
    CAMLparam1 (index);
    CAMLreturn(Val_int(RT_buildFreeVarType(Int_val(index))));
}

