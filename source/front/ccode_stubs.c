//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
#include <string.h>
#include "front_c.h"
#include "query_c.h"
#include "../simulator/abstmachine.h"
#include "../simulator/builtins/builtins.h"
#include "../simulator/dataformats.h"
#include "../simulator/hnorm.h"
#include "../simulator/io-datastructures.h"
#include "../simulator/mcstring.h"
#include "../simulator/mctypes.h"
#include "../simulator/printterm.h"
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
void c_setTypeAndTermLocation(value v)
{
    CAMLparam1 (v);
    QUERY_setTypeAndTermLocation();
    CAMLreturn0;
}

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
    QUERY_setQueryFreeVariables();
    CAMLreturn0;
}

Boolean c_queryHasVars(value v)
{
    CAMLparam1 (v);
    if (QUERY_queryHasVars()) CAMLreturn(Val_int(1));
    else CAMLreturn(Val_int(0));
}


/***************************************************************************/
/*                               read term                                 */
/***************************************************************************/

int c_initLocalTabs(value numFvs, value numTyFvs, value numTermArgs, 
                     value numTypeArgs)
{
    CAMLparam4 (numFvs, numTyFvs, numTermArgs, numTypeArgs);
    CAMLreturn(Val_int(RT_initLocalTabs(Int_val(numFvs), Int_val(numTyFvs),
                                        Int_val(numTermArgs), 
                                        Int_val(numTypeArgs))));
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

/***************************************************************************/
/*                           build (OCaml) term                            */
/***************************************************************************/


int c_numQueryVars(value unit)
{
    CAMLparam1(unit);
    PRINT_setQueryFreeVariables();
    CAMLreturn(Val_int(IO_freeVarTabTop));
}

value c_resetFreeVarTab(value unit)
{
  CAMLparam1(unit);
  PRINT_resetFreeVarTab();
  CAMLreturn(Val_unit);
}

value c_getSubTerm(value i)
{
    CAMLparam1 (i);
    DF_TermPtr tm_ptr;
    tm_ptr = IO_freeVarTab[Int_val(i)].rigdes;
    HN_lnorm(tm_ptr);
    //    PRINT_printTerm(tm_ptr);
    CAMLreturn((value) tm_ptr);
}

int c_getTermTag(value i)
{
    CAMLparam1 (i);
    DF_TermPtr tmptr = DF_termDeref((DF_TermPtr) i);
    CAMLreturn(Val_int(DF_termTag(tmptr)));
} 

int c_getDisSet(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(prs, pr, cons);

  DF_DisPairPtr liveList = AM_llreg;
  prs = Val_emptylist;    

  while (DF_isNEmpDisSet(liveList)) {
    cons = caml_alloc(2,0);
    pr = caml_alloc(2,3);
    DF_TermPtr tm1 = DF_disPairFirstTerm(liveList);
    DF_TermPtr tm2 = DF_disPairSecondTerm(liveList);
    HN_lnorm(tm1);
    HN_lnorm(tm2);
    Store_field(pr, 0, (value) tm1);
    Store_field(pr, 1, (value) tm2);
    Store_field(cons, 0, pr);
    Store_field(cons, 1, prs);
    prs = cons;
    liveList = DF_disPairNext(liveList);
  }

  CAMLreturn(prs);
}

/* constants */
int c_getConstData(value v)
{
  CAMLparam1(v);
  DF_TermPtr tm_ptr = DF_termDeref((DF_TermPtr) v);
  CAMLreturn(Val_int(DF_constTabIndex(tm_ptr)));
}

/* free variables */
int c_getFVarData(value v)
{
  CAMLparam1(v);
  DF_TermPtr tm_ptr = DF_termDeref((DF_TermPtr) v);
  int fvind = 0;

  IO_freeVarTab[IO_freeVarTabTop].rigdes = tm_ptr;
  while (tm_ptr != IO_freeVarTab[fvind].rigdes) fvind++;
  if ((fvind == IO_freeVarTabTop) && (IO_freeVarTabTop == IO_MAX_FREE_VARS)) {
    EM_error(BI_ERROR_TYFVAR_CAP);
  }
  CAMLreturn(Val_int(fvind));
}

int c_setFVarName(value index, value name)
{
  CAMLparam2(index, name);
  char* str = String_val(name);
  int len = caml_string_length(name);

  DF_StrDataPtr fvname = (DF_StrDataPtr) EM_malloc(sizeof(Word)*(MCSTR_numWords(len) + DF_STRDATA_HEAD_SIZE));
  DF_mkStrDataHead((MemPtr)fvname);
  MCSTR_toString((MemPtr)((MemPtr)fvname + DF_STRDATA_HEAD_SIZE), str, len);

  IO_freeVarTab[Int_val(index)].varName = fvname;
  IO_freeVarTabTop++;
  CAMLreturn(Val_unit);
}

/* bound variables */
int c_getBVarData(value v)
{
  CAMLparam1(v);
  DF_TermPtr tm_ptr = DF_termDeref((DF_TermPtr) v);
  CAMLreturn(Val_int(DF_bvIndex(tm_ptr)));
}

/* abstractions */
value c_getAbsData(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  DF_TermPtr tm_ptr = DF_termDeref((DF_TermPtr) v);
  res = caml_alloc(2,0);
  Store_field(res, 0, Val_int(DF_lamNumAbs(tm_ptr)));
  Store_field(res, 1, (value) (DF_lamBody(tm_ptr)));
  CAMLreturn(res);
}

/* applications */
value c_getAppData(value v)
{
  CAMLparam1(v);
  CAMLlocal3(args, cons, res);
  DF_TermPtr tm_ptr = DF_termDeref((DF_TermPtr) v);

  int numArgs = DF_appArity(tm_ptr);
  DF_TermPtr arg = DF_appArgs(tm_ptr);
  int i;
  
  for(i = numArgs; i>0; i--){
    arg++;
  }

  args = Val_emptylist;
  for (i = numArgs; i > 0; i--){
    arg--;
    cons = caml_alloc(2,0);
    Store_field(cons, 0, (value) arg);
    Store_field(cons, 1, args);
    args = cons;
  }

  res = caml_alloc(3,0);
  DF_TermPtr head = DF_termDeref(DF_appFunc(tm_ptr));
  Store_field(res, 0, (value) head);
  Store_field(res, 1, args);
  Store_field(res, 2, Val_int(numArgs));
  CAMLreturn(res);
}


/* get the tag values for term categories */
int c_getConstTag(value v)
{
  CAMLparam1 (v);
  CAMLreturn(Val_int(DF_TM_TAG_CONST));
}

int c_getFVarTag(value v)
{
  CAMLparam1 (v);
  CAMLreturn(Val_int(DF_TM_TAG_VAR));
}

int c_getBVarTag(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_int(DF_TM_TAG_BVAR));
}

int c_getAbsTag(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_int(DF_TM_TAG_LAM));
}

int c_getAppTag(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_int(DF_TM_TAG_APP));
}
